module PhonData ( elementTable
                , buildTables        
                ) where

import Database.HDBC.Sqlite3
import Database.HDBC

import Data.Phonology
import Site

data LexItem = Lx Int String

getMargins :: LexItem -> [(Int, String, String)]
getMargins (Lx langid form) = [(langid, getOns f, getCod f) | f <- words form' ]
    where
      form' = filter (`notElem` "()- /◦") form
      getOns = fstOnset ipaVowels
      getCod = lstCoda ipaVowels

insertRecords insertOnset insertCoda (langid, ons, cod) = do
  execute insertOnset [toSql langid, toSql ons]
  execute insertCoda [toSql langid, toSql cod]
  return ()

elementTable :: (IConnection conn) => conn -> Int -> Int -> IO [String]
elementTable conn elementType langid = 
    quickQuery conn "SELECT element FROM onsets_codas WHERE langid=? AND element_type=? AND element!=''" 
                   [toSql langid, toSql elementType] >>= return . reverse . sortByLength . map fromSql . concat

syllabifyLanguage :: Int -> IO ()
syllabifyLanguage langid = do
  conn <- connectDB
  onsets <- elementTable conn 0 langid
  codas <- elementTable conn 1 langid
  quickQuery' conn "SELECT form FROM reflexes WHERE langid=?" [toSql langid] >>=
             mapM_ (putStrLn . showPhonClass . parseWord onsets codas . filter (`notElem` "()-◦") . fromSql) . concat
  --disconnect conn
  return ()

buildTables = do 
  conn <- connectDB
  run conn "DROP TABLE IF EXISTS onsets_codas" []
  run conn "CREATE TABLE \"onsets_codas\" (\"langid\" integer NOT NULL, \"element\" text NOT NULL, \"element_type\" integer NOT NULL, CONSTRAINT \"element_unique\" UNIQUE (langid, element, element_type) ON CONFLICT IGNORE)" []
  insertOnset <- prepare conn "INSERT INTO onsets_codas (langid, element, element_type) VALUES (?, ?, 0)"
  insertCoda <- prepare conn "INSERT INTO onsets_codas (langid, element, element_type) VALUES (?, ?, 1)"
  items <- quickQuery conn "SELECT langid, form FROM reflexes" [] >>= 
       mapM_ (insertRecords insertOnset insertCoda) 
       . concatMap getMargins 
       . map (\[lid, fm] -> Lx (fromSql lid) (fromSql fm))
  commit conn
  disconnect conn
  return ()
