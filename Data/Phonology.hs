module Data.Phonology ( ipaVowels
                      , combiningDiacritics
                      , fstOnset
                      , lstCoda
                      , knownOnsets
                      , knownCodas
                      , sortByLength
                      , splitSo
                      , hasPrefixIn
                      , Word (..)
                      , Syl (..)
                      , Rhy (..)
                      , parseSyllable
                      , parseWord
                      , consClasses
                      , bestMatch
                      , PhonClass (..)
                      ) where

import Data.List (intercalate, takeWhile, nub, sortBy, isPrefixOf, stripPrefix, intersect, minimumBy)
import Data.List.Split (split, splitOn, startsWithOneOf)
import Data.Maybe (fromJust)

class PhonClass a where
    showPhonClass :: a -> String

data Phrase = Phrase [Word]
instance Show Phrase where
    show (Phrase xs) = unwords $ map show xs

data Word = Word [Syl]
instance Show Word where
    show (Word xs) = intercalate "-" $ map show xs
instance PhonClass Word where
    showPhonClass (Word xs) = intercalate "-" $ map showPhonClass xs

data Syl = Syl String Rhy
instance Show Syl where
    show (Syl ons rhy) = ons ++ (show rhy)
instance PhonClass Syl where
    showPhonClass (Syl ons rhy) =
        (substituteAll (classToMap consClasses) (stripCharsForComp ons)) 
        ++ (showPhonClass rhy)

data Rhy = Rhy String String
instance Show Rhy where
    show (Rhy nuc cod) = nuc ++ cod
instance PhonClass Rhy where
    showPhonClass (Rhy nuc cod) = 
        (substituteAll (classToMap vowelClasses) (stripCharsForComp nuc)) 
        ++ (substituteAll (classToMap consClasses) (stripCharsForComp cod))

splitOnSpace :: String -> [String]
splitOnSpace = splitOn " "

-- [y] has been removed because of transcriptional ambiguity
ipaVowels :: [String]
ipaVowels = splitOnSpace "i ɨ ʉ ɯ u ɪ ʏ ʊ e ø ɘ ɵ ɤ o ə ɛ ɶ ɜ ɞ ʌ ɔ æ ɐ a ɶ ɑ ɒ"

combiningDiacritics = ['\x0300' .. '\x036F']
modifierLetters = ['\x02B0' .. '\x02FF']

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

splitSo :: (String -> Bool) -> (String -> Bool) -> String -> (String, String)
splitSo fPref fSuff = splitSoSuffix' ""
    where
      splitSoSuffix' :: String -> String -> (String, String)
      splitSoSuffix' acc (x:xs)
          | fPref (x:xs) && fSuff (acc)  = (acc, (x:xs))
          | otherwise = splitSoSuffix' (acc ++ [x]) xs
      splitSoSuffix' acc [] = (acc, [])
                            
parseSyllable :: [String] -> [String] -> String -> (Syl, String)
parseSyllable onsets codas wd = (Syl (reverse ons) (Rhy (reverse nuc) (reverse cod)), afterOns)
    where
      (cod, afterCod) = splitPrefix codas wd
      (nuc, afterNuc) = splitSo (hasPrefixIn onsets) ((>0) . length .  intersect (concat ipaVowels)) afterCod
      (ons, afterOns) = splitPrefix onsets afterNuc

parseWord :: [String] -> [String] -> String -> Word
parseWord onsets codas = Word . reverse . parseWord' . reverse
    where
      parseWord' :: String -> [Syl]
      parseWord' wd' = case parseSyllable onsets codas wd' of
                        (syl, []) -> [syl]
                        (syl, wd'') -> (syl : parseWord' wd'')

stripCharsForComp :: [Char] -> [Char]
stripCharsForComp = stripChars (combiningDiacritics ++ modifierLetters)

substituteAll :: [(String, String)] -> String -> String
substituteAll subs [] = ""
substituteAll subs xs = case substitute subs xs of
                          (hd, tl) -> hd ++ (substituteAll subs tl)

substitute :: [(String,String)] -> String -> (String, String)
substitute [] (x:xs) = ([x], xs)
substitute ((s,lab):subs) form
               | s `isPrefixOf` form = (lab, fromJust $ stripPrefix s form)
               | otherwise = substitute subs form

distBetweenSyls :: Syl -> Syl -> Int
distBetweenSyls a b = dist (showPhonClass a) (showPhonClass b)

bestMatch :: Syl -> [Syl] -> Int
bestMatch s syls = fst 
                   $ minimumBy (\a b -> compare (snd a) (snd b)) 
                   $ zipWith (\i syl -> (i, distBetweenSyls s syl)) [0..] syls

classToMap :: [(String, [String])] -> [(String, String)]
classToMap = reverse . sortBy (\a b -> compare (length $ fst a) (length $ fst b)) 
             . concatMap (\(lab, xs) -> [(x, lab) | x <- xs])

consClasses = [ ("P", words "p b ɓ")
              , ("T", words "t d ɗ ʈ ɖ")
              , ("K", words "k g ɠ q ɢ ʛ c ɟ tɕ dʑ tʃ dʒ")
              , ("M", words "m hm mh")
              , ("N", words "n hn nh ɳ ɳh hɳ")
              , ("G", words "ŋ hŋ ŋh")
              , ("S", words "s z ʃ ʒ ʂ ʐ ç ʝ ʑ ɕ")
              , ("F", words "ɸ β θ ð f v x ɣ ħ ʕ")
              , ("L", words "l ɬ ɭ lh hl")
              , ("R", words "r ɾ ɽ ɹ ɺ hr rh")
              , ("W", words "w hw wh")
              , ("Y", words "j hj y hy yh")
              , ( "", words "ː ʔ h ɦ")
              ]

vowelClasses = [ ("A", words "ə ɤ ʌ æ ɐ a ɑ ɒ ɘ ɞ ɶ ɜ əə ɤɤ ʌʌ ææ ɐ aa ɑɑ ɒɒ ɘɘ ɞɞ ɶɶ ɜɜ")
               , ("I", words "i ɨ ɪ e ɛ y j ii ɨɨ ɪɪ ee ɛɛ yy")
               , ("U", words "ʉ ɯ u ʏ ʊ ø ɵ o ɔ w ʉʉ ɯɯ uu ʏʏ ʊʊ øø ɵɵ oo ɔɔ")
               , ( "", words "ː ə")
               ]

dist :: Eq a => [a] -> [a] -> Int
dist a b 
    = last (if lab == 0 then mainDiag
	    else if lab > 0 then lowers !! (lab - 1)
		 else{- < 0 -}   uppers !! (-1 - lab))
    where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
	  uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
	  lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
	  eachDiag a [] diags = []
	  eachDiag a (bch:bs) (lastDiag:diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
	      where nextDiag = head (tail diags)
	  oneDiag a b diagAbove diagBelow = thisdiag
	      where doDiag [] b nw n w = []
		    doDiag a [] nw n w = []
		    doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
			where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
		    firstelt = 1 + head diagBelow
		    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
	  lab = length a - length b
          min3 x y z = if x < y then x else min y z