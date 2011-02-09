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


cogsetSelect' :: [(String, String)] -> JQSelect
cogsetSelect' params = jqSelect 
                       [ "langid", "name" ]
                      [ SelectFieldAs "GROUP_CONCAT(form, ';;;')" "forms"
                      , SelectFieldAs "GROUP_CONCAT(gloss, ';;;')" "glosses"
                      , SelectFieldAs "GROUP_CONCAT(refid, ';;;')" "refids"
                      , SelectFieldAs "GROUP_CONCAT(morph_index, ';;;')" "morphinds" ]
                      (SelectSelect langs)
                      [ JnSelUsing JnPlain reflexes ["langid"]]
                      []
                      ["langid"]
                      [("langgrp","ASC"), ("langsubgrp","ASC"), ("name", "ASC")]
                      SelectLimitNone
                      (params)
                          where
                            reflexes = jqSelect 
                                       ["langid", "prefid", "form", "gloss", "refid", "morph_index" ]
                                       []
                                       (SelectSource "reflexes")
                                       [ JnUsing JnPlain "reflex_of" ["refid"] ]
                                       [ WhEqNum "prefid" $ read $ fromJust $ lookup "prefid" params ]
                                       []
                                       [] -- Order by
                                       SelectLimitNone
                                       []
                            langs = jqSelect 
                                    ["langid", "name", "langgrp", "langsubgrp"]
                                    []
                                    (SelectSource "descendant_of")
                                    [JnUsing JnPlain "langnames" ["langid"]]
                                    [WhEqNum "plangid" $ read $ fromJust $ lookup "plangid" params, WhTrue "display"]
                                    []
                                    []
                                    SelectLimitNone
                                    params


reflexesSelect' :: [(String, String)] -> JQSelect
reflexesSelect' params = jqSelect' reflexes params
    where
      plangid = read $ fromJust $ lookup "plangid" params
      strFields = ["form", "gloss", "langid", "langgrp"]
      wheres = [WhLike f v | (f, v) <- params, f `elem` strFields]
      reflexes = defaultJQSelect 
                 { selectFields = SelectFields $ (map SelectField [ "refid", "form", "gloss" ]) ++ 
                   [SelectFieldAs "descendant_of.langid" "langid"] ++ 
                   (map SelectField [ "cogmorph", "langgrp" ])
                 , selectSource = SelectSource "descendant_of"
                 , selectJoins = SelectJoins [ JnUsing JnPlain "reflexes" ["langid"]
                                             , JnOn JnPlain "langnames" "langnames.langid" "descendant_of.langid"
                                             , JnSelOn JnLeft reflexOf "refid" "ro_refid" 
                                             ]
                 , selectWhere = SelectWhere $ WhAnd $ [WhEqNum "plangid" plangid] ++ wheres
                 }
      reflexOf = defaultJQSelect
                 { selectFields = SelectFields [ SelectFieldAs "plangid" "ro_plangid"
                                               , SelectFieldAs "refid" "ro_refid"
                                               , SelectFieldAs "GROUP_CONCAT(morph_index || \":\" || prefid)" "cogmorph" 
                                               ]
                 , selectSource = SelectSource "reflex_of"
                 , selectWhere = SelectWhere $ WhAnd [WhEqNum "ro_plangid" plangid] 
                 , selectGroup = SelectGroup ["refid"]
                 }

protoSelect' :: [(String, String)] -> JQSelect
protoSelect' params = jqSelect' proto params
    where
      plangid = read $ fromMaybe "0" $ lookup "langid" params
      strFields = ["form", "gloss", "numref"]
      wheres = [WhLike f v | (f, v) <- params, f `elem` strFields]
      proto = defaultJQSelect 
              { selectFields = SelectFields $ map SelectField [ "refid", "form", "gloss", "numref" ]
              , selectSource = SelectSource "reflexes"
              , selectJoins = SelectJoins [JnSelUsing JnLeft numRef ["refid"]]
              , selectWhere = SelectWhere $ WhAnd 
                              $ [WhEqNum "langid" plangid] ++ wheres
              }
      numRef = defaultJQSelect
               { selectFields = SelectFields [SelectFieldAs "prefid" "refid", SelectFieldAs "COUNT(*)" "numref"]
               , selectSource = SelectSource "reflex_of"
               , selectGroup = SelectGroup ["prefid"]
               }

getSingleReflex :: (IConnection conn) => conn -> [(String, String)] -> IO JSValue
getSingleReflex conn params = do
  quickQuery' conn "SELECT form, gloss FROM reflexes WHERE refid=?" [nToSql refid] >>= 
              return . makeObj . zip ["form","gloss"] 
                              . map (showJSON . (fromSql :: SqlValue -> String)) . concat
      where
        refid = read $ fromJust $ lookup "refid" params

-- Builds a hash-table (as a JSON object) from an SQL table given a
-- table name, the field to use for the key, and the field to use as
-- the value.
getJSONMap :: String -> String -> String -> String -> IO JSValue
getJSONMap table key value wh = withDbConnection getJSONMap'
    where
      getJSONMap' conn = quickQuery' conn sql [] >>= return . makeObj . map (\[k,v] -> (fromSql k, showJSON v))
      sql = printf "SELECT DISTINCT %s, %s FROM %s %s" key value table wh

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
  qType <- getInput "qtype"
  conn <- liftIO connectDB
  json <- getInputs >>= liftIO . handleSqlError . (chooseType conn qType) . map (mapPair decodeString)
  liftIO $ disconnect conn
  setHeader "Content-Type" "application/json; charset=utf-8"
  output $ encodeString $ encode $ json
      where
        chooseType conn qType = 
            case qType of
              Just "langnames" ->  \_ -> (getJSONMap "langnames" "langid" "name" "WHERE display ORDER BY langgrp, name")
              Just "plangnames" ->  \_ -> (getJSONMap "langnames JOIN descendant_of ON langnames.langid=descendant_of.plangid" 
                                                          "plangid" "name" "ORDER BY name")
              Just "cogset" -> getCogSetJSON
              Just "cogsets" -> jqSelectResp conn $ protoSelect'
              Just "reflexes" -> jqSelectResp conn $ reflexesSelect'
              Just "single" -> getSingleReflex conn
              _ -> \_ -> return $ showJSON "No qtype given."

main :: IO ()
main = runCGI $ handleErrors $ cgiMain
