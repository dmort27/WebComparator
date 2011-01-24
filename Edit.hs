module Main where

import Network.CGI
import Network.JQGrid

import Text.JSON
import Text.Printf

import qualified Data.List.Split as Split

import Database.HDBC

import Data.Maybe (fromMaybe, fromJust)
import Data.List (intercalate)

import Codec.Binary.UTF8.String (encodeString, decodeString)
import qualified Data.ByteString.UTF8

import Site

data TableInfo = TableInfo { tableUniqueId :: String
                           , tableEditableFields :: [String]
                           }

addRecord table inputs = return JSNull

tableInfo = [ ("reflexes", TableInfo "refid" ["form","gloss","langid"])
            , ("cogsets", TableInfo "cogsetid" ["form","gloss"])
            ]

actionAdd :: IConnection conn => [(String, String)] -> String -> conn -> IO JSValue
actionAdd inputs table conn = do
  withTransaction conn $ \c -> run c sql params
  [[id]] <- quickQuery' conn "SELECT last_insert_rowid()" []
  return $ showJSON id
    where
      tableInfo' = fromJust $ lookup table tableInfo
      uniqueId = tableUniqueId tableInfo'
      fields = tableEditableFields tableInfo'
      editPairs =  [(k, v) | (k, v) <- inputs, k `elem` fields]
      sql = printf "INSERT INTO %s (%s) VALUES (%s)" 
            table 
            (intercalate "," $ map fst editPairs) 
            (intercalate "," $ map (\_ -> "?") editPairs) :: String
      params = map (toSql . snd) editPairs

actionUpdate :: (IConnection conn) => [(String, String)] -> String -> conn -> IO JSValue
actionUpdate inputs table conn = do
  withTransaction conn $ \c -> run c sql params
  return $ makeObj [("sql", showJSON sql),("params", showJSON params)]
    where
      tableInfo' = fromJust $ lookup table tableInfo
      uniqueId = tableUniqueId tableInfo'
      fields = tableEditableFields tableInfo'
      id = read $ fromJust $ lookup "id" inputs :: Integer
      editPairs =  [(k, v) | (k, v) <- inputs, k `elem` fields]
      updates = intercalate "," $ map (\(k, v) -> printf "%s=?" k :: String) editPairs
      sql = printf "UPDATE %s SET %s WHERE %s=?" table updates uniqueId :: String
      params = map (toSql . snd) editPairs ++ [toSql id]

actionDelete :: (IConnection conn) => [(String, String)] -> String -> conn -> IO JSValue
actionDelete inputs table conn = withTransaction conn $ \c -> run c sql params 
                                 >> return (makeObj [("sql", showJSON sql), ("params", showJSON params)])
    where
      tableInfo' = fromJust $ lookup table tableInfo
      uniqueId = tableUniqueId tableInfo'
      id = read $ fromJust $ lookup "id" inputs
      sql = printf "DELETE FROM %s WHERE %s=?" table uniqueId
      params = [SqlInteger id]
    
actionAddToSet :: (IConnection conn) => [(String, String)] -> conn -> IO JSValue
actionAddToSet inputs conn = do
  withTransaction conn $ \c -> run c sql params
  return JSNull
      where
        sql = "INSERT INTO reflex_of (refid, cogsetid, morph_index) VALUES (?, ?, ?)"
        refid = read $ fromJust $ lookup "refid" inputs
        cogsetid = read $ fromJust $ lookup "cogsetid" inputs
        morphInd = read $ fromMaybe "0" $ lookup "morphind" inputs
        params = map SqlInteger [refid, cogsetid, morphInd]

actionRemoveFromSet :: (IConnection conn) => [(String, String)] -> conn -> IO JSValue
actionRemoveFromSet inputs conn = withTransaction conn $ \c -> run c sql params >> return JSNull
      where
        sql = "DELETE FROM reflex_of WHERE refid=? AND cogsetid=?"
        refid = read $ fromJust $ lookup "refid" inputs
        cogsetid = read $ fromJust $ lookup "cogsetid" inputs
        params = map SqlInteger [refid, cogsetid]

actionSetMorphInd :: (IConnection conn) => [(String, String)] -> conn -> IO JSValue
actionSetMorphInd inputs conn =  withTransaction conn $ \c -> run c sql params >> return JSNull
      where
        sql = "UPDATE reflex_of SET morph_index=? WHERE refid=? AND cogsetid=?"
        refid = read $ fromJust $ lookup "refid" inputs
        cogsetid = read $ fromJust $ lookup "cogsetid" inputs
        morphInd = read $ fromJust $ lookup "morphind" inputs
        params = map SqlInteger [morphInd, refid, cogsetid]

withDbConnection action = handleSqlError $ do
      conn <- connectDB
      result <- action conn
      disconnect conn
      return result

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (a, b) = (f a, f b)

cgiMain :: CGIT IO CGIResult
cgiMain = do
  setHeader "Content-Type" "application/json; charset=utf-8"
  oper <- getInput "oper"
  table <- getInput "table"
  json <- getInputs >>= liftIO . (handle oper table) . map (mapPair decodeString)
  output $ encodeString $ encode $ json
      where
        handle oper table inputs =
            case (oper, table) of
              (Just "add", Just table') -> withDbConnection (actionAdd inputs table')
              (Just "edit", Just table') -> withDbConnection (actionUpdate inputs table')
              (Just "del", Just table') -> withDbConnection (actionDelete inputs table')
              (Just "addtoset", Nothing) -> withDbConnection (actionAddToSet inputs)
              (Just "setmorphind", Nothing) -> withDbConnection (actionSetMorphInd inputs)
              (Just "removefromset", Nothing) -> withDbConnection (actionRemoveFromSet inputs)
              (_, _) -> return $ showJSON "No operation executed."
            
main :: IO ()
main = runCGI $ handleErrors $ cgiMain
