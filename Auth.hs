module Main where

import Network.CGI

import Text.Printf

import Data.Time
import Data.Time.Clock.POSIX

import Database.HDBC

import Site

withDbConnection action = handleSqlError $ do
      conn <- connectDB
      result <- action conn
      disconnect conn
      return result

actionGetUID :: IConnection conn => String -> String -> conn -> IO (Maybe Integer)
actionGetUID username secret conn = 
    quickQuery' conn sql params >>= return . 
                    \r -> case r of
                            [[n]] -> Just $ fromSql n
                            _ -> Nothing
    where
      sql = "SELECT uid FROM users WHERE username=? AND secret=?"
      params = map toSql [username, secret]

actionGetIdentifier :: IConnection conn => Integer -> conn -> IO String
actionGetIdentifier uid conn = 
    do
      time <- getPOSIXTime
      run conn insertSql [toSql uid, toSql time]
      quickQuery' conn selectSql [toSql uid, toSql time] >>= return . fromSql . (!!0) . (!!0)
          where
            insertSql = "INSERT INTO cookies (uid, issued, identifier) VALUES (?, ?, lower(hex(randomblob(16))))"
            selectSql = "SELECT identifier FROM cookies WHERE uid=? AND time=?"            

cgiMain :: CGIT IO CGIResult
cgiMain = do
  setHeader "Content-Type" "application/json; charset=utf-8"
  username <- getInput "username"
  secret <- getInput "secret"
  output ""
  
main :: IO ()
main = runCGI $ handleErrors $ cgiMain
