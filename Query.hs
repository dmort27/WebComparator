module Main where

import Network.CGI
import Network.JQGrid

import Text.JSON
import Text.Printf

import Data.List (intercalate)
import Data.Maybe (fromMaybe, fromJust)

import qualified Data.List.Split as Split

import Database.HDBC
import Database.HDBC.Sqlite3

import Codec.Binary.UTF8.String (encodeString, decodeString)
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

{-
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
-}

{-
-- Deprecated: cogsets table phrase out. Fix.
cogSetSQL = "SELECT langid, name, GROUP_CONCAT(form, ';;;') AS forms, " ++ 
            "GROUP_CONCAT(gloss, ';;;') AS glosses, " ++
            "GROUP_CONCAT(refid, ';;;') AS refids, " ++ 
            "GROUP_CONCAT(morph_index, ';;;') AS morphinds " ++ 
            "FROM langnames " ++ 
            "LEFT NATURAL JOIN (SELECT langid, cogsetid, form, gloss, refid, morph_index FROM reflexes " ++
            "NATURAL JOIN reflex_of WHERE cogsetid=?) GROUP BY langid"
-}

convertDelimList :: SqlValue -> JSValue
convertDelimList SqlNull = JSArray []
convertDelimList x = showJSON $ map showJSON $ Split.splitOn ";;;" (fromSql x :: String)

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
                                                      
getCogSetJSON :: [(String, String)] -> IO JSValue
getCogSetJSON inputs = do
  case lookup "prefid" inputs of
    Just prefid -> 
        handleSqlError $ do
                         conn <- connectDB
                         json <- quickQuery' conn (show $ cogsetSelect' inputs) [] >>= return . cogSetRespToJSON
                         disconnect conn
                         return json
                         
    Nothing -> return $ showJSON "Error! No cogsetid given."

cogsetSelect :: [(String, String)] -> JQSelect
cogsetSelect params = jqSelect (SelectSource "langnames")
                      [ "langid", "name" ]
                      [ SelectFieldAs "GROUP_CONCAT(form, ';;;')" "forms"
                      , SelectFieldAs "GROUP_CONCAT(gloss, ';;;')" "glosses"
                      , SelectFieldAs "GROUP_CONCAT(refid, ';;;')" "refids"
                      , SelectFieldAs "GROUP_CONCAT(morph_index, ';;;')" "morphinds" ]
                      [ JnSelUsing JnLeft reflexes ["langid"]]
                      [ WhTrue "display" ]
                      [ "langid" ]
                      [("langgrp","ASC"), ("name","ASC")] -- Order by
                      (params)
                          where
                            plangid = read $ fromJust $ lookup "plangid" params :: Int
                            reflexes = jqSelect (SelectSource "reflexes")
                                       ["langid", "prefid", "form", "gloss", "refid", "morph_index" ]
                                       []
                                       [JnUsing JnPlain "reflex_of" ["refid"]]
                                       [WhEqNum "prefid" $ read $ fromJust $ lookup "prefid" params]
                                       []
                                       [] -- Order by
                                       []

cogsetSelect' :: [(String, String)] -> JQSelect
cogsetSelect' params = jqSelect (SelectJQSelect langs)
                       [ "langid", "name" ]
                      [ SelectFieldAs "GROUP_CONCAT(form, ';;;')" "forms"
                      , SelectFieldAs "GROUP_CONCAT(gloss, ';;;')" "glosses"
                      , SelectFieldAs "GROUP_CONCAT(refid, ';;;')" "refids"
                      , SelectFieldAs "GROUP_CONCAT(morph_index, ';;;')" "morphinds" ]
                      [ JnSelUsing JnLeft reflexes ["langid"]]
                      []
                      ["langid"]
                      [("name", "ASC")]
                      (params)
                          where
                            reflexes = jqSelect (SelectSource "reflexes")
                                       ["langid", "prefid", "form", "gloss", "refid", "morph_index" ]
                                       []
                                       [ JnUsing JnPlain "reflex_of" ["refid"] ]
                                       [ WhEqNum "prefid" $ read $ fromJust $ lookup "prefid" params ]
                                       []
                                       [] -- Order by
                                       []
                            langs = jqSelect (SelectSource "descendant_of")
                                    ["langid", "name"]
                                    []
                                    [JnUsing JnPlain "langnames" ["langid"]]
                                    [WhEqNum "plangid" $ read $ fromJust $ lookup "plangid" params, WhTrue "display"]
                                    []
                                    []
                                    params

reflexesSelect :: [(String, String)] -> JQSelect
reflexesSelect params = jqSelect (SelectSource "reflexes")
                        [ "refid", "form", "gloss" ] 
                        [ SelectFieldAs "reflexes.langid" "langid"
                        , SelectFieldAs "GROUP_CONCAT(morph_index || \":\" || prefid)" "cogmorph"] 
                        [ JnUsing JnLeft "reflex_of" ["refid"]
                        , JnOn JnLeft "langnames" "langnames.langid" "reflexes.langid"] 
                        ([WhTrue "display"] ++ wheres)
                        ["refid"] 
                        [] -- Order by
                        params
    where
      strFields = ["form", "gloss"]
      numFields = ["refid", "langid"]
      wheres = [WhLike (relativize f) v | (f, v) <- params, f `elem` strFields] ++
               [WhEqNum (relativize f) (read v) | (f, v) <- params, f `elem` numFields]
      relativize "langid" = "reflexes.langid"
      relativize x = x

protoSelect :: [(String, String)] -> JQSelect
protoSelect params = jqSelect (SelectSource "reflexes")
              ["refid", "form", "gloss"]
              []
              []
              ([WhEqNum "langid" plangid] ++ wheres)
              []
              [] -- Order by
              params
    where
      plangid = read $ fromMaybe "0" $ lookup "langid" params
      strFields = ["form", "gloss"]
      wheres = [WhLike f v | (f, v) <- params, f `elem` strFields]

-- Builds a hash-table (as a JSON object) from an SQL table given a
-- table name, the field to use for the key, and the field to use as
-- the value.
getJSONMap :: String -> String -> String -> String -> IO JSValue
getJSONMap table key value wh = withDbConnection getJSONMap'
    where
      getJSONMap' conn = quickQuery' conn sql [] >>= return . makeObj . map (\[k,v] -> (fromSql k, showJSON v))
      sql = printf "SELECT DISTINCT %s, %s FROM %s %s" key value table wh

-- Returns data suitable for populating a JQGrid based on a JQGridQuery object.
getJSONGrid :: JQGridQuery -> IO JSValue
getJSONGrid query = withDbConnection (\conn -> jqGridResponse conn query)

-- Perform the specified action with a DB connection. Queries must
-- force strict evaluation; lazy queries will not return any values
-- before the connection is closed. The function should be moved to
-- another module so it is not duplicated.
withDbConnection :: (Connection -> IO a) -> IO a
withDbConnection action = handleSqlError $ do
      conn <- connectDB
      result <- action conn
      disconnect conn
      return result

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a, b) = (f a, f b)

-- The main function running in the CGI monad. It passes the arguments
-- to different functions generating JSON based on the value of qtype.
cgiMain :: CGIT IO CGIResult
cgiMain = do
  setHeader "Content-Type" "application/json; charset=utf-8"
  qType <- getInput "qtype"
  conn <- liftIO connectDB
  json <- getInputs >>= liftIO . handleSqlError .
          (case qType of
             Just "langnames" ->  \_ -> (getJSONMap "langnames" "langid" "name" "WHERE display ORDER BY langgrp, name")
             Just "plangnames" ->  \_ -> (getJSONMap "langnames JOIN descendant_of ON langnames.langid=descendant_of.plangid" 
                                                         "plangid" "name" "ORDER BY name")
             Just "cogset" -> getCogSetJSON
             Just "cogsets" -> jqSelectResp conn $ protoSelect
             Just "reflexes" -> jqSelectResp conn $ reflexesSelect
             Just _ -> jqSelectResp conn $ reflexesSelect
             Nothing -> jqSelectResp conn $ reflexesSelect
          )
  liftIO $ disconnect conn
  output $ encodeString $ encode $ json

main :: IO ()
main = runCGI $ handleErrors $ cgiMain
