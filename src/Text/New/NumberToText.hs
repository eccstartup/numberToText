module Text.New.NumberToText
       (
        numberToText,
        digitToText,
        numberToEnglish,
        digitToEnglish
       ) where

import Data.List.Split
import Data.Char
import qualified Data.List.Split.Internals as SI
import Data.String.Utils

-- | useless later
-- | given an Integer n, show its words in English
-- | it works for minus numbers
numberToText :: Integer -> String
numberToText n
  | n < 0 = "minus " ++ numberToText (-n)
  | t == 1 = toText1 n
  | t == 2 = toText2 n
  | t == 3 = toText3 n
  | n > (10^3006) = fail "too long number"
  | otherwise = toText (toTriples t) n
  where t = bitWidth n

-- | useless later
-- | number of digits
bitWidth n = length $ show n

-- | one digit number
toText1 n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"

-- | two digit number
toText2 n
  | n < 10 = toText1 n
  | n == 10 = "ten"
  | n == 11 = "eleven"
  | n == 12 = "twelve"
  | n == 13 = "thirteen"
  | n == 14 = "fourteen"
  | n == 15 = "fifteen"
  | n == 16 = "sixteen"
  | n == 17 = "seventeen"
  | n == 18 = "eighteen"
  | n == 19 = "nighteen"
  | n == 20 = "twenty"
  | n == 30 = "thirty"
  | n == 40 = "forty"
  | n == 50 = "fifty"
  | n == 60 = "sixty"
  | n == 70 = "seventy"
  | n == 80 = "eighty"
  | n == 90 = "ninety"
  | otherwise = toText2 (n - (mod n 10)) ++ "-" ++ toText1 (mod n 10)

-- | three digit number
toText3 n
  | n < 100 = toText2 n
  | mod n 100 == 0 = toText1 (div n 100) ++ " hundred"
  | otherwise = toText3 (n - (mod n 100)) ++ " and " ++ toText2 (mod n 100)

-- | useless later
toText bit n
  | bit == 3 = toText3 n
  | n < 10^(bit - 3) = toText (bit - 3) n
  | (mod n $ 10^(bit - 3)) == 0 = toText 3 (div n $ 10^(bit - 3)) ++ " " ++ numText (bit - 3)
  | otherwise = toText bit (n - (mod n $ 10^(bit - 3))) ++ ", " ++ toText (bit - 3) (mod n $ 10^(bit - 3))

-- | useless later
numText bit
  | t == 0 = "thousand"
  | t == 1 = "million"
  | t == 2 = "billion"
  | t == 3 = "trillion"
  | t == 4 = "quadrillion"
  | t == 5 = "quintillion"
  | t == 6 = "sextillion"
  | t == 7 = "septillion"
  | t == 8 = "octillion"
  | t == 9 = "nonillion"
  | t == 1000 = "milliatillion"
  | t > 1000 = fail "too large number"
  | otherwise = afix !! a ++ cfix !! c ++ bfix !! b ++ xfix !! c
  where t = quot (bit - 3) 3
        -- | a of 134 == 1
        a = div t 100
        -- | c of 134 == 4
        c = mod t 10
        -- | b of 134 == 3
        b = mod (div t 10) 10
        afix = ["", "cen", "duocen", "trecen", "quadringen", "quingen", "sescen", "septingen", "octingen", "nongen"]
        cfix = ["", "un", "duo", "tre", "quattuor", "quin", "sex", "septen", "octo", "novem"]
        bfix = ["", "dec", "vigin", "trigin", "quadragin", "quinquagin", "sexagin", "septuagin", "octogin", "nonagin"]
        xfix = ["", "illion", "tillion", "tillion", "tillion", "tillion", "tillion", "tillion", "tillion", "tillion"]

-- | useless later
-- | toTriples 5 = 6
-- | toTriples 6 = 6
toTriples n
  | mod n 3 == 0 = n
  | otherwise = n + 3 - (mod n 3)

------------------------------------------------------------
------------------------------------------------------------

-- | given a float number, show its words in English
-- | also works for minus float
digitToText :: String -> String
digitToText d
  | head d == '-'  = "minus " ++ digitToText (tail d)
  | not $ elem '.' d = numberToText a
  | otherwise = numberToText a ++ " point " ++ digitsText b
                where a = read (head $ splitOn "." d) :: Integer
                      b = last $ splitOn "." d

-- | given a string with all digits, output a string
digitsText :: String -> String
digitsText b = putTogether s
           where s = map (numberToText . toInteger . (\x -> x-48) . ord) b

-- | put together all the words separated with a space
putTogether :: [String] -> String
putTogether [] = []
putTogether [x] = x
putTogether (x:xs) = x ++ " " ++ putTogether xs

------------------------------------------------------------
------------------------------------------------------------

numberToEnglish :: String -> String
numberToEnglish word = replace ", zero" "" $ numberToEnglish1 word

numberToEnglish1 :: String -> String
numberToEnglish1 word
  | head word == '-'  = "minus " ++ numberToEnglish1 (tail word)
  | length word1 == 0 = "zero"
  | length word1 <= 3 = toText3 $ readInt1 word1
  | otherwise = helper1 n (head words) ++ numberToEnglish1 (concat $ tail words)
  where words = splitWord word1
        word1 = dropWhile (=='0') word
        n = length words - 1
        helper1 n w
          | w == "000" = ""
          | otherwise = toText3 (readInt1 w) ++ " " ++ thousandEnglish n ++ ", "

splitWord :: String -> [String]
splitWord word = map reverse $ reverse $ (SI.chunksOf 3) $ reverse word

-- | ["123", "456"] to Integer [123, 456]
readInt1 :: String -> Int
readInt1 s = read s

-- | ["123", "456"] to Integer [123, 456]
readInts :: [String] -> [Int]
readInts s = map read s

thousandEnglish t
  | t <= 1000 = thousandEnglish1 t
  | t > 1000 = thousandEnglish2 t

thousandEnglish1 t
  | t == 0 = ""
  | t == 1 = "thousand"
  | t == 2 = "million"
  | t == 3 = "billion"
  | t == 4 = "trillion"
  | t == 5 = "quadrillion"
  | t == 6 = "quintillion"
  | t == 7 = "sextillion"
  | t == 8 = "septillion"
  | t == 9 = "octillion"
  | t == 10 = "nonillion"
  | t == 1000 = "milliatillion"
  | t < 100 =  afix !! a ++ cfix !! c ++ bfix !! b ++ xfix !! b
  | otherwise = afix !! a ++ cfix !! c ++ bfix !! b ++ xfix1 !! b
  where
        -- | a of 134 == 1
        a = div t 100
        -- | c of 134 == 4
        c = mod t 10
        -- | b of 134 == 3
        b = mod (div t 10) 10
        afix = ["", "cen", "duocen", "trecen", "quadringen", "quingen", "sescen", "septingen", "octingen", "nongen"]
        cfix = ["", "un", "duo", "tre", "quattuor", "quin", "sex", "septen", "octo", "novem"]
        bfix = ["", "dec", "vigin", "trigin", "quadragin", "quinquagin", "sexagin", "septuagin", "octogin", "nonagin"]
        xfix = ["", "illion", "tillion", "tillion", "tillion", "tillion", "tillion", "tillion", "tillion", "tillion"]
        xfix1 = ["tillion", "illion", "tillion", "tillion", "tillion", "tillion", "tillion", "tillion", "tillion", "tillion"]

thousandEnglish2 t = (concat $ zipWith helper2 aa indexes) ++ "tillion"
                 where aa = zipWith (-) (repeat kk) [0..kk]
                       kk = length indexes - 1
                       indexes = readInts $ splitWord $ show t

-- | k times "millia", n < 1000
helper2 k n = afix !! a ++ cfix !! c ++ bfix !! b ++ rep k
        where
              -- | a of 134 == 1
              a = div n 100
              -- | c of 134 == 4
              c = mod n 10
              -- | b of 134 == 3
              b = mod (div n 10) 10
              afix = ["", "cen", "duocen", "trecen", "quadringen", "quingen", "sescen", "septingen", "octingen", "nongen"]
              cfix = ["", "un", "duo", "tre", "quattuor", "quin", "sex", "septen", "octo", "novem"]
              bfix = ["", "dec", "vigin", "trigin", "quadragin", "quinquagin", "sexagin", "septuagin", "octogin", "nonagin"]
              --xfix = ["", "tillion", "tillion", "tillion", "tillion", "tillion", "tillion", "tillion", "tillion", "tillion"]
              thou = "millia"
              rep t = concat $ replicate t thou

-- | given a float String, show its words in English
-- | also works for minus float String
digitToEnglish :: String -> String
digitToEnglish d
  | head d == '-'  = "minus " ++ digitToText (tail d)
  | not $ elem '.' d = numberToEnglish a
  | otherwise = numberToEnglish a ++ " point " ++ digitsText b
                where a = head $ splitOn "." d
                      b = last $ splitOn "." d
