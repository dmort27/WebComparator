module Main where

import Network.CGI
import Network.JQGrid
import Data.Phonology

import Text.JSON
import Text.Printf

import qualified Data.List.Split as Split

import Database.HDBC

import Data.Maybe (fromMaybe, fromJust)
import Data.List (intercalate, intersect)
import Data.List.Split (splitOn)

import Codec.Binary.UTF8.String (encodeString, decodeString)
import qualified Data.ByteString.UTF8

import PhonData
import Site

data TableInfo = TableInfo 
    { tableName :: String
    , tableUniqueId :: String
    , tableEditableFields :: [String]
    }

-- | Information about tables for the two objects of edits.
tableInfo :: [(String, TableInfo)]
tableInfo = [ ( "reflexes"
              , TableInfo "reflexes" "refid" ["form","gloss","langid"] )
            , ( "cogsets"
              , TableInfo "reflexes" "refid" ["form","gloss","langid"] ) ]

-- | Given an association list of parameters, a table name, and a
-- database connection, @actionAdd@ inserts a new into the approprate
-- table and returns a JSON object containing the SQL used for the
-- transaction.
actionAdd :: IConnection conn => [(String, String)] -> String -> conn -> IO JSValue
actionAdd inputs table conn = do
  withTransaction conn $ \c -> run c sql params
  [[id]] <- quickQuery' conn "SELECT last_insert_rowid()" []
  return $ makeObj [("sql", showJSON sql), ("params", makeObj [(k, showJSON v) | (k, v) <- editPairs])]
    where
      tableInfo' = fromJust $ lookup table tableInfo
      uniqueId = tableUniqueId tableInfo'
      fields = tableEditableFields tableInfo'
      editPairs =  [(k, v) | (k, v) <- inputs, k `elem` fields]
      sql = printf "INSERT INTO %s (%s) VALUES (%s)" 
            (tableName tableInfo')
            (intercalate "," $ map fst editPairs) 
            (intercalate "," $ map (\_ -> "?") editPairs) :: String
      params = map (toSql . snd) editPairs

-- | Given an association list of parameters, a table name, and a
-- database connection, @actionUpdate@ updates a database record and
-- returns a JSON object containing the update paramaters and the SQL
-- used for the transaction.
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
      sql = printf "UPDATE %s SET %s WHERE %s=?" (tableName tableInfo') updates uniqueId :: String
      params = map (toSql . snd) editPairs ++ [toSql id]

-- | Given an association list of parameters, a table name, and a
-- database connection, @actionDelete@ deletes a rows from the
-- database. It returns a JSON object containing the SQL query and the
-- parameters passed to the script.
actionDelete :: (IConnection conn) => [(String, String)] -> String -> conn -> IO JSValue
actionDelete inputs table conn = 
    withTransaction conn $ \c -> run c sql params 
    >> return (makeObj [("sql", showJSON sql), ("params", showJSON params)])
        where
          tableInfo' = fromJust $ lookup table tableInfo
          uniqueId = tableUniqueId tableInfo'
          id = read $ fromJust $ lookup "id" inputs
          sql = printf "DELETE FROM %s WHERE %s=?" (tableName tableInfo') uniqueId
          params = [SqlInteger id]
    
actionAddToSet :: (IConnection conn) => [(String, String)] -> conn -> IO JSValue
actionAddToSet inputs conn = do
  onsets <- elementTable conn 0 (fromIntegral langid)
  codas <- elementTable conn 1 (fromIntegral langid)
  let parser = parseWord onsets codas

  ponsets <- elementTable conn 0 (fromIntegral plangid)
  pcodas <- elementTable conn 1 (fromIntegral plangid)
  let pparser = parseWord ponsets pcodas

  let params = map SqlInteger [ refid
                              , prefid
                              , plangid
                              , fromIntegral $ getBestMorphInd parser pparser protoForm form
                              ]
  withTransaction conn $ \c -> run c updateSql [ toSql $ show $ parser form
                                               , toSql refid 
                                               ]
  withTransaction conn $ \c -> run c sql params
  return JSNull
      where
        sql = "INSERT INTO reflex_of (refid, prefid, plangid, morph_index) VALUES (?, ?, ?, ?)"
        updateSql = "UPDATE reflexes SET form=? WHERE refid=?"
        langid = read $ fromJust $ lookup "langid" inputs
        refid = read $ fromJust $ lookup "refid" inputs
        form = filter (`notElem` "-") $ fromJust $ lookup "form" inputs
        plangid = read $ fromJust $ lookup "plangid" inputs
        prefid = read $ fromJust $ lookup "prefid" inputs
        protoForm = fromJust $ lookup "protoform" inputs
        protoGloss = fromJust $ lookup "protogloss" inputs        

actionAddGroupToSet :: (IConnection conn) => [(String, String)] -> conn -> IO JSValue
actionAddGroupToSet inputs conn = do
  ponsets <- elementTable conn 0 (fromIntegral plangid)
  pcodas <- elementTable conn 1 (fromIntegral plangid)
  let pparser = parseWord ponsets pcodas
  mapM_ (addItem pparser) refids

  return JSNull

      where
        insertSql = "INSERT INTO reflex_of (refid, prefid, plangid, morph_index) VALUES (?, ?, ?, ?)"
        reflexSql = "SELECT langid, form FROM reflexes WHERE refid=?"
        updateSql = "UPDATE reflexes SET form=? WHERE refid=?"
        refids = (map read) $ splitOn "," $ fromJust $ lookup "refids" inputs
        plangid = read $ fromJust $ lookup "plangid" inputs
        prefid = read $ fromJust $ lookup "prefid" inputs
        protoForm = fromJust $ lookup "protoform" inputs
        protoGloss = fromJust $ lookup "protogloss" inputs

        addItem pparser refid = do
          [[langid, form]] <- quickQuery' conn reflexSql [toSql refid]
          onsets <- elementTable conn 0 (fromSql langid)
          codas <- elementTable conn 1 (fromSql langid)
          let parser = parseWord onsets codas
          let form = filter (`notElem` "-") $ fromJust $ lookup "form" inputs
          let params = map SqlInteger [ refid
                                      , prefid
                                      , plangid
                                      , fromIntegral $ getBestMorphInd parser pparser protoForm form
                                      ]

          withTransaction conn $ \c -> run c updateSql [ toSql $ show $ parser form
                                                       , toSql refid 
                                                       ]
          withTransaction conn $ \c -> run c insertSql params
          return ()
          

getBestMorphInd :: (String -> Word) -> (String -> Word) -> String -> String -> Int
getBestMorphInd parser pparser protoform form = bestMatch protoform' form'
    where
      Word (protoform':_) = pparser 
                            $ head 
                            $ filter (\x -> (x `intersect` (concat ipaVowels)) /= []) 
                            $ splitOn "-" protoform
      Word form' = parser form

actionRemoveFromSet :: (IConnection conn) => [(String, String)] -> conn -> IO JSValue
actionRemoveFromSet inputs conn = withTransaction conn $ \c -> run c sql params >> return JSNull
      where
        sql = "DELETE FROM reflex_of WHERE refid=? AND prefid=?"
        refid = read $ fromJust $ lookup "refid" inputs
        prefid = read $ fromJust $ lookup "prefid" inputs
        params = map SqlInteger [refid, prefid]

actionSetMorphInd :: (IConnection conn) => [(String, String)] -> conn -> IO JSValue
actionSetMorphInd inputs conn =  withTransaction conn $ \c -> run c sql params >> return JSNull
      where
        sql = "UPDATE reflex_of SET morph_index=? WHERE refid=? AND prefid=?"
        refid = read $ fromJust $ lookup "refid" inputs
        prefid = read $ fromJust $ lookup "prefid" inputs
        morphInd = read $ fromJust $ lookup "morphind" inputs
        params = map SqlInteger [morphInd, refid, prefid]

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
