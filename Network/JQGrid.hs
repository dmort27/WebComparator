-- Implements a Haskell interface to the JQGrid table widget using Database.HDBC and Data.JSON.
module Network.JQGrid ( JQGridQuery
              , JQGridTableRow
              , JQGridResp
              , recordCount
              , pageCount
              , startRecord
              , toJQGridTableRow
              , jqGridQuery
              , jqGridResponse ) where

import System.Locale
import System.Time

import qualified Data.ByteString.UTF8
import qualified Codec.Binary.UTF8.String

import Database.HDBC
import Database.HDBC.Sqlite3
import Text.JSON
import Text.Printf (printf)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

-- Instances allowing the serialization of items of type SqlValue
-- (from Database.HDBC) as JSON values.
instance JSON SqlValue where
    showJSON x@(SqlString x') = fromAmbiguous x'
    showJSON x@(SqlByteString x') = fromAmbiguous $ Data.ByteString.UTF8.toString x'
    showJSON x@(SqlWord32 _) = showJSON (fromSql x :: Integer)
    showJSON x@(SqlWord64 _) = showJSON (fromSql x :: Integer)
    showJSON x@(SqlInteger _) = showJSON (fromSql x :: Integer)
    showJSON x@(SqlChar c) = showJSON ([c])
    showJSON x@(SqlBool _) = showJSON (fromSql x :: Bool)
    showJSON x@(SqlDouble _) = showJSON (fromSql x :: Double)
    showJSON x@(SqlRational _) = showJSON (fromSql x :: Double)
    showJSON x@(SqlLocalDate _) = showJSON (fromSql x :: String)
    showJSON x@(SqlLocalTimeOfDay _) = showJSON (fromSql x :: String)
    showJSON x@(SqlZonedLocalTimeOfDay _ _) = showJSON (fromSql x :: String)
    showJSON x@(SqlLocalTime _) = showJSON (fromSql x :: String)
    showJSON x@(SqlZonedTime _) = showJSON (fromSql x :: String)
    showJSON x@(SqlUTCTime _) = showJSON (fromSql x :: String)
    showJSON x@(SqlDiffTime _) = showJSON (fromSql x :: Double)
    showJSON x@(SqlPOSIXTime t) = showJSON $ formatCalendarTime defaultTimeLocale "%Y-%m-%d" (fromSql x)
    showJSON x@(SqlEpochTime t) = showJSON $ formatCalendarTime defaultTimeLocale "%Y-%m-%d" (fromSql x)
    showJSON x@(SqlTimeDiff t) = showJSON x
    showJSON _ = JSNull
    readJSON x = return SqlNull -- Not implemented

readMay :: Read a => String -> Maybe a
readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
                [x] -> Just x
                _ -> Nothing

fromAmbiguous :: String -> JSValue
fromAmbiguous x = 
    case (readMay x) :: (Maybe Double) of
      Just n -> showJSON n
      Nothing -> showJSON x

-- Class for representing queries from the JQGrid widget to the database
data JQGridQuery = JQGridQuery
    { queryTable :: String
    , queryFields :: [String]
    , querySpecFields :: [String]
    , queryPage :: Int
    , queryLimit :: Int
    , querySidx :: String
    , querySord :: String
    , queryWheres :: [(String,String)]
    , querySQL :: String -- SQL schema for printf with %s for fields, where clause, sord, sidx, offset, and limit
    } deriving (Show, Eq)

-- Class for representing records from the database to be sent back to
-- the JQGrid widget JSON objects to be presented as table rows.
data JQGridTableRow
    = JQGridTableRow [(String, SqlValue)] | JQGridTableRowError
      deriving (Show, Eq)

instance JSON JQGridTableRow where
    showJSON (JQGridTableRow xs) = makeObj [(a, showJSON b) | (a, b) <- xs]
    readJSON _ = return JQGridTableRowError

-- Class of responses from the database to be encoded as JSON and sent
-- to the JQGrid widget.
data JQGridResp = JQGridResp 
    { respTotal :: Int
    , respPage :: Int
    , respRecords :: Int
    , respRows :: [JQGridTableRow]
    } deriving (Show, Eq)


instance JSON JQGridResp where
    showJSON resp 
        = makeObj 
          [ ( "total", showJSON $ respTotal resp )
          , ( "page", showJSON $ respPage resp )
          , ( "records", showJSON $ respRecords resp )
          , ( "rows", showJSON $ respRows resp )
          ]
    readJSON x = return $ JQGridResp 0 0 0 [] -- Not implemented

-- Given a connection to an HDBC database and the name of a table,
-- returns the number of records in the table.
recordCount :: (IConnection conn) => conn -> String -> IO Int
recordCount conn tableName = do
  [[n]] <- quickQuery conn ("SELECT COUNT(*) AS COUNT FROM " ++ tableName) []
  return $ fromSql n

-- Utility function for computing the number of pages in a table,
-- given records (per page) and limit (number of records per page).
pageCount :: (Integral a) => a -> a -> a
pageCount records limit = 
    if (records > 0) then ceiling $ (fromIntegral records) / (fromIntegral limit) else 0

-- Utility function for computing the first record to be returned in a
-- query given limit (the number of recrods per page) and page (the
-- page to be returned).
startRecord :: (Integral a) => a -> a -> a
startRecord limit page = limit * (page - 1)

-- Given a list of field names and a list of SqlValue from HDBC,
-- returns an equivalent JQGridTableRow.
toJQGridTableRow :: [String] -> [SqlValue] -> JQGridTableRow
toJQGridTableRow fields row = JQGridTableRow $ zip fields row

-- Given the name of the table, the name of the table's fields, a skeleton SQL
-- string for building a query, and a list of HTTP property-value
-- pairs, returns a JQGridQuery.
jqGridQuery :: String  -> [String] -> [String] -> String -> [(String,String)] -> JQGridQuery
jqGridQuery table fields specFields sql inputs = JQGridQuery
    { queryTable = table
    , queryFields = fields
    , querySpecFields = specFields
    , queryPage = page
    , queryLimit = rows
    , querySidx = sidx
    , querySord = sord
    , queryWheres = wheres
    , querySQL = sql
    }
    where
      page = read $ fromMaybe "1" $ lookup "page" inputs 
      rows = read $ fromMaybe "100" $ lookup "rows" inputs 
      sidx = fromMaybe "id" $ lookup "sidx" inputs 
      sord = fromMaybe "ASC" $ lookup "sord" inputs 
      wheres = [(k, Codec.Binary.UTF8.String.decodeString v) | (k,v) <- inputs, k `elem` fields]

-- Implements basic communication between a JQGrid widget and an HDBC
-- database backend. Takes an HDBC database connection and a
-- JQGridQuery object and returns a response serialized as JSON
-- interpretable by the JQGrid widget.
jqGridResponse :: (IConnection conn) => conn -> JQGridQuery -> IO String
jqGridResponse conn query = do
  records <- recordCount conn (queryTable query)
  let totalPages = pageCount records (queryLimit query)
  let page' = if (queryPage query <= totalPages) 
              then (queryPage query) 
              else totalPages
  rows <- quickQuery' conn sql []
  return $ encode $ showJSON $ 
         JQGridResp totalPages page' records 
                        (map (toJQGridTableRow ((queryFields query) ++ querySpecFields query)) rows)

      where
        start = startRecord (queryLimit query) (queryPage query)
        wheres' = map (\(k,v) -> printf "%s LIKE '%%%s%%'" k v) (queryWheres query)
        whereClause = if (wheres' == []) 
                      then "" 
                      else " WHERE " ++ (intercalate " AND " wheres') ++ " "
        sql = printf (querySQL query) -- "SELECT %s FROM %s %s ORDER BY %s %s LIMIT %d, %d" 
              (intercalate "," $ queryFields query) -- Fields
              (queryTable query) -- Table name
              whereClause        -- Where clause
              (querySidx query)  -- Sort index
              (querySord query)  -- Sort order ("ASC" or "DESC")
              start              -- offset
              (queryLimit query) -- rows per page

-- Samples for testing:

sqlSample1 = "SELECT %s, GROUP_CONCAT(morph_index || \":\" || cogsetid) " ++
             "FROM %s LEFT NATURAL JOIN reflex_of %s GROUP BY refid ORDER BY %s %s LIMIT %d, %d"

fieldsSample1 = ["refid", "form", "gloss", "langid"] 
