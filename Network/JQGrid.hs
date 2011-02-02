-- | Implements a Haskell interface to the JQGrid table widget using Database.HDBC and Data.JSON.
module Network.JQGrid ( JQGridQuery (..)
                      , SelectFields (..)
                      , SelectField (..)
                      , SelectSource (..)
                      , SelectJoin (..)
                      , JnType (..)
                      , SelectJoins (..)
                      , SelectCond (..)
                      , SelectWhere (..)
                      , SelectOrder (..)
                      , SelectGroup (..)
                      , SelectLimit (..)
                      , JQSelect (..)
                      , defaultJQSelect
                      , jqSelect
                      , jqSelectResp
                      , JQGridTableRow (..)
                      , JQGridResp (..)
                      , recordCount
                      , pageCount
                      , startRecord
                      , toJQGridTableRow
                      , jqGridQuery
                      , jqGridResponse ) where

import System.Locale
import System.Time

import qualified Data.ByteString.UTF8

import Database.HDBC
import Database.HDBC.Sqlite3
import Text.JSON
import Text.Printf (printf)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, fromJust)

makeSafe :: String -> String
makeSafe = filter (`notElem` ";.,'\"")

parenthesize :: String -> String
parenthesize s = "(" ++ s ++ ")"

data SelectFields = SelectFields [SelectField]
instance Show SelectFields where
    show (SelectFields xs) = "SELECT " ++ (intercalate ", " $ map show xs)

data SelectField = SelectField String | SelectFieldAs String String
instance Show SelectField where
    show (SelectField f) = f
    show (SelectFieldAs f f') = f ++ " AS " ++ f'

data SelectSource = SelectSource String | SelectSelect JQSelect
instance Show SelectSource where
    show (SelectSource table) = "FROM " ++ table
    show (SelectSelect jqSelect) = "FROM (" ++ (show jqSelect) ++ ")"

data JnType = JnPlain | JnLeft | JnLeftOuter | JnInner | JnCross
instance Show JnType where
    show JnPlain = ""
    show JnLeft = "LEFT"
    show JnLeftOuter = "LEFT OUTER"
    show JnInner = "LEFT INNER"
    show JnCross = "CROSS"

data SelectJoin = JnNat JnType String
                | JnSelNat JnType JQSelect
                | JnOn JnType String String String
                | JnSelOn JnType JQSelect String String
                | JnUsing JnType String [String]
                | JnSelUsing JnType JQSelect [String]
instance Show SelectJoin where
    show (JnNat jnType table)                 = printf "NATURAL %s JOIN %s" (show jnType) table
    show (JnSelNat jnType select)             = printf "NATURAL %s JOIN (%s)" (show jnType) (show select)
    show (JnOn jnType table field field')     = printf "%s JOIN %s ON %s=%s" (show jnType) table field field'
    show (JnSelOn jnType select field field') = printf "%s JOIN (%s) ON %s=%s" (show jnType) (show select) field field'
    show (JnUsing jnType table fields)     = printf "%s JOIN %s USING (%s)" (show jnType) table (intercalate "," fields)
    show (JnSelUsing jnType select fields) = printf "%s JOIN (%s) USING (%s)" (show jnType) (show select) (intercalate "," fields)

data SelectJoins = SelectJoins [SelectJoin]
instance Show SelectJoins where
    show (SelectJoins joins) = intercalate " " $ map show joins

data SelectCond = WhEqNum String Int | WhEqStr String String | WhLike String String
                | WhNull String | WhNotNull String
                | WhTrue String | WhFalse String
                | WhAnd [SelectCond] | WhOr [SelectCond] | WhNone
                 deriving (Eq)                 
instance Show SelectCond where
    show (WhEqNum field value) = printf "%s=%d" field value
    show (WhEqStr field value) = printf "%s='%s'" field $ makeSafe value
    show (WhLike field value) = printf "%s LIKE '%%%s%%'" field $ makeSafe value
    show (WhNull field) = printf "%s IS NULL" field
    show (WhNotNull field) = printf "%s IS NOT NULL" field
    show (WhTrue field) = printf "%s" field
    show (WhFalse field) = printf "NOT %s" field
    show (WhAnd conds) = parenthesize $ intercalate " AND " $ map show conds
    show (WhOr conds) = parenthesize $ intercalate " OR " $ map show conds

data SelectWhere = SelectWhere SelectCond deriving (Eq)
instance Show SelectWhere where
    show (SelectWhere WhNone) = ""
    show (SelectWhere (WhAnd [])) = ""
    show (SelectWhere (WhOr [])) = ""
    show (SelectWhere xs) = "WHERE " ++ show xs

data SelectOrder = SelectOrder [(String, String)] | SelectOrderNone
instance Show SelectOrder where
    show SelectOrderNone = ""
    show (SelectOrder []) = ""
    show (SelectOrder xs) = "ORDER BY " ++ (intercalate ", " $ map (\(f,o) -> f ++ " " ++ o) xs)

data SelectGroup = SelectGroup [String]
instance Show SelectGroup where
    show (SelectGroup []) = ""
    show (SelectGroup xs) = "GROUP BY " ++ (intercalate ", " xs)

data SelectLimit = SelectLimit Int Int | SelectLimitNone
instance Show SelectLimit where
    show SelectLimitNone = ""
    show (SelectLimit offset limit) = printf "LIMIT %d, %d" offset limit

data JQSelect = JQSelect
    { selectFields :: SelectFields
    , selectSource :: SelectSource
    , selectJoins :: SelectJoins
    , selectWhere :: SelectWhere
    , selectGroup :: SelectGroup
    , selectOrder :: SelectOrder
    , selectLimit :: SelectLimit
    }
instance Show JQSelect where
    show select = intercalate " " $
                  filter (/="")
                  [ (show $ selectFields select)
                  , (show $ selectSource select)
                  , (show $ selectJoins select)
                  , (show $ selectWhere select)
                  , (show $ selectGroup select)
                  , (show $ selectOrder select)
                  , (show $ selectLimit select)
                  ]

defaultJQSelect = 
    JQSelect
    { selectFields = SelectFields []
    , selectSource = SelectSource ""
    , selectJoins = SelectJoins []
    , selectWhere = SelectWhere WhNone
    , selectGroup = SelectGroup []
    , selectOrder = SelectOrder []
    , selectLimit = SelectLimitNone
    }

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

-- Tries to convert Strings to JSON numerical values; if this is not
-- possible, returns a JSON string value.
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
    , respSql :: String
    } deriving (Show, Eq)

instance JSON JQGridResp where
    showJSON resp 
        = makeObj 
          [ ( "total", showJSON $ respTotal resp )
          , ( "page", showJSON $ respPage resp )
          , ( "records", showJSON $ respRecords resp )
          , ( "rows", showJSON $ respRows resp )
          , ( "sql", showJSON $ respSql resp )
          ]
    readJSON x = return $ JQGridResp 0 0 0 [] "" -- Not implemented

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

-- Deprecated. Given the name of the table, the name of the table's
-- fields, a skeleton SQL string for building a query, and a list of
-- HTTP property-value pairs, returns a JQGridQuery.
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
      wheres = [(k, v) | (k,v) <- inputs, k `elem` fields]

-- Given the name of the source table, a list of searchable fields (as
-- strings), a list of other fields (as SelectField), a list of joins
-- (as JnTable), and an association list of parameters from the HTTP
-- request, returns a JQSelect.
jqSelect :: [String]  -> [SelectField] -> SelectSource  -> [SelectJoin] -> [SelectCond] -> [String] -> [(String,String)] -> SelectLimit -> [(String,String)] -> JQSelect
jqSelect fields specFields source joins wheres groupBys orderBys limit params = JQSelect
    { selectFields = SelectFields $ (map SelectField fields) ++ specFields
    , selectSource = source
    , selectJoins = SelectJoins joins
    , selectWhere = SelectWhere $ WhAnd wheres'
    , selectGroup = SelectGroup groupBys
    , selectOrder = SelectOrder orderBys'
    , selectLimit = limit
    }
    where
      maybeSidx = lookup "sidx" params 
      maybeSord = lookup "sord" params 
      orderBys' = (case (maybeSidx, maybeSord) of
                     (Just sidx, Just sord) -> [(sidx, sord)]
                     _ -> []
                  ) ++ orderBys
      wheres' = wheres ++ [WhLike f v | (f, v) <- params, f `elem` fields]

jqSelectResp :: Connection -> ([(String,String)] -> JQSelect) -> [(String,String)] -> IO JSValue
jqSelectResp conn select params = do
  records <- recordCount conn source
  let totalPages = pageCount records rows
  let page' = if (page <= totalPages) then page else totalPages
  rows <- quickQuery' conn (show jqSelect) []
  return $ showJSON $ JQGridResp totalPages page' records (map (toJQGridTableRow fieldNames) rows) (show jqSelect)
      where
        jqSelect = select params
        SelectSource source = selectSource jqSelect
        SelectFields fields = selectFields jqSelect
        page = read $ fromJust $ lookup "page" params
        rows = read $ fromJust $ lookup "rows" params
        fieldNames = 
            map (\x -> case x of
                         SelectField f -> f
                         SelectFieldAs _ f -> f) fields
        
-- Implements basic communication between a JQGrid widget and an HDBC
-- database backend. Takes an HDBC database connection and a
-- JQGridQuery object and returns a response serialized as JSON
-- interpretable by the JQGrid widget.
jqGridResponse :: (IConnection conn) => conn -> JQGridQuery -> IO JSValue
jqGridResponse conn query = do
  records <- recordCount conn (queryTable query)
  let totalPages = pageCount records (queryLimit query)
  let page' = if (queryPage query <= totalPages) 
              then (queryPage query) 
              else totalPages
  rows <- quickQuery' conn sql []
  return $ showJSON $ 
         JQGridResp totalPages page' records 
                        (map (toJQGridTableRow ((queryFields query) ++ querySpecFields query)) rows) ""
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

fieldsSample1 = ["refid", "form", "gloss", "langid"] 
