module PhonData ( elementTable
                , buildTables        
                ) where

import Database.HDBC.Sqlite3
import Database.HDBC

import qualified System.IO.UTF8 as U8
import Generics.Pointless.Combinators ((><))

import Data.List (intercalate, sortBy, nub)
import Data.Ord (comparing)
import Data.List.Split (split, splitOn, startsWithOneOf)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

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

data ACTRow = ACTRow 
    { actID :: Int
    , actProtoForm :: String
    , actProtoOnset :: String
    , actProtoRhyme :: String
    , actProtoGloss :: String
    , actReflexes :: [[(String, Int)]]
    } deriving (Show,Eq)

type ACT = ([ACTRow], [String])

makeAbstractCogTable :: (IConnection conn) => conn -> Int -> [Int] -> IO ACT
makeAbstractCogTable conn plangid langids = do
  langnames <- quickQuery conn "SELECT langid, name FROM langnames" [] >>= return . map (fromSql . (!!1)) . filter ((`elem` langids) . fromSql . head)
  rows <- quickQuery conn sql [] >>= return . map fromRow
  return (rows, langnames)
      where
        sql = concat [ "SELECT refid as prefid, form AS protoform, gloss AS protogloss, "
                     , (intercalate ", " $ map (printf "forms%d") langids)
                     , " FROM reflexes "
                     , intercalate " " $ map (\id -> 
                                                  printf ("LEFT JOIN (SELECT prefid AS prefid%d, " ++ 
                                                          "'[' || GROUP_CONCAT( '(\"' || form || '\",' || morph_index || ')' ) || ']' AS forms%d " ++ 
                                                          "FROM reflex_of JOIN reflexes USING (refid) " ++ 
                                                          "WHERE langid=%d GROUP BY prefid%d) ON prefid=prefid%d") id id id id id) langids
                     , printf " WHERE langid=%d ORDER BY protoform" plangid
                     ]
        nullToEmpty :: SqlValue -> SqlValue
        nullToEmpty SqlNull = SqlString "[]"
        nullToEmpty x = x
        fromRow :: [SqlValue] -> ACTRow
        fromRow (prefid:protoform:protogloss:xs) = 
            ACTRow 
            { actID = fromSql prefid
            , actProtoForm = fromSql protoform
            , actProtoOnset = takeWhile (\x -> [x] `notElem` ipaVowels) 
                              (fromSql protoform)
            , actProtoRhyme = dropWhile (\x -> [x] `notElem` ipaVowels) 
                              (fromSql protoform)
            , actProtoGloss = fromSql protogloss
            , actReflexes = map (read . fromSql . nullToEmpty) xs
            }

trimToCogMorph :: (String, Int) -> (String, Int)
trimToCogMorph (wd, ind) = ( fromMaybe "IndexTooLarge" $ (!ind) $ concatMap (splitOn "-") $ splitOn " " wd, ind)

transformRow :: ((String, Int) -> (String, Int)) -> ACTRow -> ACTRow
transformRow f row = row { actReflexes = map (map f) $ actReflexes row }

sortACTByRhyme :: ACT -> ACT
sortACTByRhyme (actRows, langNames) = ( sortBy (comparing actProtoRhyme) actRows
                                      , langNames)

sortACTByOnset :: ACT -> ACT
sortACTByOnset (actRows, langNames) = ( sortBy (comparing actProtoOnset) actRows
                                      , langNames)

filterACTBy :: (Eq a) => (ACTRow -> a) -> a -> ACT -> ACT
filterACTBy accessor value (actRows, langNames) = 
    (filter ((==value) . accessor) actRows
    , langNames)

levelsInACTBy :: (Eq a) => (ACTRow -> a) -> ACT -> [a]
levelsInACTBy accessor (actRows, _) = nub $ map accessor actRows

divideACTByLevels :: (Eq a) => (ACTRow -> a) -> ACT -> [ACT]
divideACTByLevels accessor act = map (\l -> filterACTBy accessor l act) 
                                 $ levelsInACTBy accessor act

showACT :: ACT -> String
showACT = uncurry (flip (++)) . ((intercalate "\n" . map showACTRow) >< ((++ "\n") . intercalate "\t" . (["Gloss","Protoform"]++)))

showTrimmedACT :: ACT -> String
showTrimmedACT = intercalate "\n" . map (showACTRow . transformRow trimToCogMorph) . fst

showACTRow :: ACTRow -> String
showACTRow row = intercalate "\t" ([ printf "‘%s’" $ actProtoGloss row
                                   , "*" ++ actProtoForm row
                                   ] ++ reflexes)
                 where
                   reflexes = map (fst . fromMaybe ("",0) . fstElem) $ actReflexes row

fstElem :: [a] -> Maybe a
fstElem [] = Nothing
fstElem (x:xs) = Just x

trimACT :: [ACTRow] -> [ACTRow]
trimACT = map (transformRow trimToCogMorph)

(!) :: [a] -> Int -> Maybe a
(!) xs i = plingHelper xs i 0
plingHelper [] _ _ = Nothing
plingHelper (x:xs) i n
	|i == n = Just x
	|otherwise = plingHelper xs i (n+1)
