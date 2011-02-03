module Data.Phonology where

import Data.List (intercalate, takeWhile, nub, sortBy, isPrefixOf, stripPrefix)
import Data.List.Split (split, splitOn, startsWithOneOf)
import Data.Maybe (fromJust)

data Word = Word [Syl]
instance Show Word where
    show (Word xs) = intercalate "-" $ map show xs

data Syl = Syl String Rhy
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
fstOnset vowels = takeWhile (\x -> [x] `notElem` vowels)

lstCoda :: (Eq a) => [[a]] -> [a] -> [a]
lstCoda vowels = reverse . fstOnset vowels . reverse

sortByLength :: (Eq a) => [[a]] -> [[a]]
sortByLength = sortBy (\a b -> compare (length a) (length b))

knownOnsets :: (Eq a) => [[a]] -> [[a]] -> [[a]]
knownOnsets vowels = reverse . sortByLength . nub . map (fstOnset vowels)

knownCodas :: (Eq a) => [[a]] -> [[a]] -> [[a]]
knownCodas vowels = reverse . sortByLength . nub . map (lstCoda vowels)

splitOnset :: [String] -> String -> (String, String)
splitOnset [] wd = ("", wd)
splitOnset (o:os) wd | o `isPrefixOf` wd = (o, fromJust $ stripPrefix o wd)
                     | otherwise = splitOnset os wd

startsWithOnset :: [String] -> String -> Bool
startsWithOnset onsets wd = case splitOnset onsets wd of
                              ("", _) -> False
                              (_ , _) -> True

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

--parseWord1 :: onsets 

{-
parseWord :: [String] -> [String] -> String -> [Syl]
parseWord onsets codas =
-}