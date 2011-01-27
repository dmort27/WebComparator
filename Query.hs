module Main where

import Network.CGI
import Network.JQGrid

import Text.JSON
import Text.Printf

import Data.List (intercalate)
import Data.Maybe (fromMaybe)

import qualified Data.List.Split as Split

import Database.HDBC
--import Database.HDBC.Sqlite3

import Codec.Binary.UTF8.String (encodeString)
import qualified Data.ByteString.UTF8

import Site

data CogSetRow = CogSetRow 
    { csLangId :: SqlValue
    , csLangName :: SqlValue
    , csForms :: SqlValue
    , csGlosses :: SqlValue
    , csIds :: SqlValue
    , csMorphInds :: SqlValue
    } | CogSetNull deriving (Eq,Show)

instance JSON CogSetRow where
    showJSON (CogSetRow langid langname forms glosses ids morphInds) = 
        showJSON [ showJSON (fromSql langid :: Int)
                 , showJSON (fromSql langname :: String)
                 , convertDelimList forms
                 , convertDelimList glosses
                 , convertDelimList ids
                 , convertDelimList morphInds
                 ]
    readJSON _ = return CogSetNull  -- not implemented

data ReflexTableRecord = ReflexTableRecord
    { ctId :: Integer
    , ctGloss :: String
    , ctProtoForm :: String
    , ctReflexes :: [Maybe String]
    } deriving (Eq)

instance Show ReflexTableRecord where
    show (ReflexTableRecord id gloss protoform reflexes) = 
        (intercalate "\t" $ [show id, ("*"++protoform)] ++ (map (fromMaybe "---") reflexes)) ++ "\n"

data RTRs = RTRs [ReflexTableRecord]

instance Show RTRs where
    show (RTRs xs) = concatMap show xs

reflexRowToRecord :: [Integer] -> [SqlValue] -> ReflexTableRecord
reflexRowToRecord langIds (id:gloss:protoform:reflexes) = 
    ReflexTableRecord { ctId = fromSql id
                      , ctGloss = fromSql gloss
                      , ctProtoForm = fromSql protoform
                      , ctReflexes = map (getCognatePart . fromSql) reflexes
                      }
    where
      getCognatePart refl = 
          case refl of
            Just refl' -> Just $ (!! read ind) $ concat $ map (Split.splitOn "-") $ words form
                where [form, ind] = Split.splitOn ";;;" refl'
            Nothing -> Nothing

-- Deprecated: cogsets table phased out. Fix.
createReflexTable :: (IConnection conn) => conn -> [Integer] -> IO Integer
createReflexTable conn langs = run conn sql []
    where
      sql = printf " CREATE TEMPORARY TABLE reflextable AS SELECT cogsets.cogsetid, gloss, form AS protoform, %s FROM cogsets %s"
            (intercalate "," $ map (printf "form%d") langs)
            (intercalate " " $ map (\l -> printf ("NATURAL LEFT OUTER JOIN " ++ 
                                                 "(SELECT cogsetid, (form || ';;;' || morph_index) AS form%d " ++ 
                                                 "FROM reflex_of NATURAL JOIN reflexes WHERE langid=%d) AS lang%d")
                                    l l l) langs)

-- Deprecated: cogsets table phrase out. Fix.
reflexRecords :: [Integer] -> IO RTRs
reflexRecords langs = do
  conn <- connectDB
  createReflexTable conn langs
  records <- quickQuery' conn "SELECT * FROM reflextable" [] >>= return . map (reflexRowToRecord langs)
  disconnect conn
  return $ RTRs records

convertDelimList :: SqlValue -> JSValue
convertDelimList SqlNull = JSArray []
convertDelimList x = showJSON $ map showJSON $ Split.splitOn ";;;" (fromSql x :: String)

-- Deprecated: cogsets table phrase out. Fix.
cogSetSQL = "SELECT langid, name, GROUP_CONCAT(form, ';;;') AS forms, " ++ 
            "GROUP_CONCAT(gloss, ';;;') AS glosses, " ++
            "GROUP_CONCAT(refid, ';;;') AS refids, " ++ 
            "GROUP_CONCAT(morph_index, ';;;') AS morphinds " ++ 
            "FROM langnames " ++ 
            "LEFT NATURAL JOIN (SELECT langid, cogsetid, form, gloss, refid, morph_index FROM reflexes " ++
            "NATURAL JOIN reflex_of WHERE cogsetid=?) GROUP BY langid"

cogSetRespToJSON :: [[SqlValue]] -> JSValue
cogSetRespToJSON  = 
    showJSON . map (\[langid, langname, forms, glosses, ids, morphInds] ->
        showJSON $ CogSetRow 
                     { csLangId = langid
                     , csLangName = langname
                     , csForms = forms
                     , csGlosses = glosses 
                     , csIds = ids
                     , csMorphInds = morphInds
                     })
                                                      
getCogSetJSON :: [(String, String)] -> IO String
getCogSetJSON inputs = do
  case lookup "cogsetid" inputs of
    Just cogsetid -> 
        handleSqlError $ do
                         conn <- connectDB
                         json <- quickQuery' conn cogSetSQL [toSql (read cogsetid :: Integer)] >>= return . encode . cogSetRespToJSON
                         disconnect conn
                         return json
                         
    Nothing -> return $ encode $ showJSON "Error! No cogsetid given."

reflexesSQL = "SELECT %s, GROUP_CONCAT(morph_index || \":\" || cogsetid) AS cogmorph " ++ -- %s for fields
              "FROM %s LEFT NATURAL JOIN reflex_of %s " ++ -- %s for table, %s for where clause
              "GROUP BY refid ORDER BY %s %s LIMIT %d, %d" -- %s for sidx, %s for sord, %d for offset, %d for row limit
reflexesFields = ["refid", "form", "gloss", "langid"]
reflexesDefaults = [("sidx","gloss")]

reflexQuery :: [(String, String)] -> JQGridQuery
reflexQuery inputs = jqGridQuery "reflexes" reflexesFields ["cogmorph"] reflexesSQL (inputs ++ reflexesDefaults)

protoFormSQL = "SELECT %s FROM %s %s ORDER BY %s %s LIMIT %d, %d"
protoFormFields = ["cogsetid", "form", "gloss"]
protoFormDefaults = [("sidx","gloss")]

protoFormQuery :: [(String, String)] -> JQGridQuery
protoFormQuery inputs = jqGridQuery "cogsets" protoFormFields [] protoFormSQL (inputs ++ protoFormDefaults)

getJSONGrid :: JQGridQuery -> IO String
getJSONGrid query = 
    handleSqlError $ do
      conn <- connectDB
      json <- jqGridResponse conn query
      disconnect conn
      return json

getJSONMap :: String -> String -> String -> IO String
getJSONMap table key value =
    handleSqlError $ do
      conn <- connectDB
      json <- quickQuery' conn sql [] >>= return . encode . makeObj . map (\[k,v] -> (fromSql k, showJSON v))
      disconnect conn
      return json
          where
            sql = printf "SELECT %s, %s FROM %s" key value table

cgiMain :: CGIT IO CGIResult
cgiMain = do
  setHeader "Content-Type" "application/json; charset=utf-8"
  qType <- getInput "qtype"
  json <- getInputs >>= liftIO .
          case qType of
            Just "langnames" ->  \_ -> (getJSONMap "langnames" "langid" "name")
            Just "cogset" -> getCogSetJSON
            Just "cogsets" -> getJSONGrid . protoFormQuery
            Just "reflexes" -> getJSONGrid . reflexQuery
            Just _ -> getJSONGrid . reflexQuery -- Should return an error message instead
            Nothing -> getJSONGrid . reflexQuery -- Should return an error message instead
  output $ encodeString $ json

main :: IO ()
main = runCGI $ handleErrors $ cgiMain
