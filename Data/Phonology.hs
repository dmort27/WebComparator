module Data.Phonology where

import Data.List (intercalate, takeWhile, nub, sortBy, isPrefixOf, stripPrefix)
import Data.List.Split (split, splitOn, startsWithOneOf)
import Data.Maybe (fromJust)

data Word = Word [Syl]
instance Show Word where
    show (Word xs) = intercalate "-" $ map show xs

data Syl = Syl String Rhy | WdBnd
instance Show Syl where
    show (Syl ons rhy) = ons ++ (show rhy)

data Rhy = Rhy String String
instance Show Rhy where
    show (Rhy nuc cod) = nuc ++ cod

splitOnSpace :: String -> [String]
splitOnSpace = splitOn " "

ipaVowels :: [String]
ipaVowels = splitOnSpace "i y ɨ ʉ ɯ u ɪ ʏ ʊ e ø ɘ ɵ ɤ o ə ɛ ɶ ɜ ɞ ʌ ɔ æ ɐ a ɶ ɑ ɒ"

combiningDiacritics = ['\x0300'..'\x036F']

stripChars :: (Eq a) => [a] -> [a] -> [a]
stripChars chars = filter (`notElem` chars)

strippedWords :: [String] -> [String]
strippedWords = map (stripChars combiningDiacritics)

fstOnset :: (Eq a) => [[a]] -> [a] -> [a]
fstOnset vowels = reverse . takeWhile (\x -> [x] `notElem` vowels)

lstCoda :: (Eq a) => [[a]] -> [a] -> [a]
lstCoda vowels = fstOnset vowels . reverse

sortByLength :: (Eq a) => [[a]] -> [[a]]
sortByLength = sortBy (\a b -> compare (length a) (length b))

knownOnsets :: (Eq a) => [[a]] -> [[a]] -> [[a]]
knownOnsets vowels = sortByLength . nub . map (fstOnset vowels)

knownCodas :: (Eq a) => [[a]] -> [[a]] -> [[a]]
knownCodas vowels = sortByLength . nub . map (lstCoda vowels)

splitPrefix :: [String] -> String -> (String, String)
splitPrefix [] wd = ("", wd)
splitPrefix (x:xs) wd 
    | x `isPrefixOf` wd = (x, fromJust $ stripPrefix x wd)
    | otherwise         = splitPrefix xs wd

hasPrefixIn :: [String] -> String -> Bool
hasPrefixIn onsets wd = any (\x -> x `isPrefixOf` wd) onsets

splitSoSuffix :: (String -> Bool) -> String -> (String, String)
splitSoSuffix f = splitSoSuffix' ""
    where
      splitSoSuffix' :: String -> String -> (String, String)
      splitSoSuffix' acc (x:xs)
          | f (x:xs) = (acc, (x:xs))
          | otherwise = splitSoSuffix' (acc ++ [x]) xs
      splitSoSuffix' acc [] = (acc, [])
                            

parseSyllable :: [String] -> [String] -> String -> (Syl, String)
parseSyllable onsets codas wd = (Syl (reverse ons) (Rhy (reverse nuc) (reverse cod)), afterOns)
    where
      (cod, afterCod) = splitPrefix codas wd
      (nuc, afterNuc) = splitSoSuffix (hasPrefixIn onsets) afterCod
      (ons, afterOns) = splitPrefix onsets afterNuc

parseWord :: [String] -> [String] -> String -> Word
parseWord onsets codas = Word . reverse . parseWord' . reverse
    where
      parseWord' :: String -> [Syl]
      parseWord' wd' = case parseSyllable onsets codas wd' of
                        (syl, []) -> [syl]
                        (syl, wd'') -> (syl : parseWord' wd'')

{-

splitRhyme :: [String] -> [String] -> String -> (String, String)
splitRhyme onsets vowels wd = splitRhyme' ("", wd)
    where
      splitRhyme' :: (String, String) -> (String, String)
      splitRhyme' (rhy, xs) | startsWithOnset onsets xs = 
                                case splitOnset onsets xs of
                                  (o, "") -> (rhy ++ xs, "")
                                  (o, xs') -> if [head xs'] `elem` vowels then (rhy, xs) else (rhy ++ xs, "")
                            | xs == [] = (rhy, xs)
                            | otherwise = splitRhyme' (rhy ++ [head xs], tail xs)

-}

--parseWord1 :: onsets 

{-
parseWord :: [String] -> [String] -> String -> [Syl]
parseWord onsets codas =
-}