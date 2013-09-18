module Text.New.NumberToText
       (
        numberToText,
        digitToText
       ) where

import Data.List.Split
import Data.Char


-- | given an Integer n, show its words in English
-- | it works for minus numbers
numberToText :: Integer -> String
numberToText n
  | n < 0 = "minus " ++ numberToText (-n)
  | bitWidth n == 1 = toText1 n
  | bitWidth n == 2 = toText2 n
  | bitWidth n == 3 = toText3 n
  | bitWidth n < 7 = toText456 n
  | bitWidth n < 10 = toText789 n
  | bitWidth n < 12 = toText101112 n
  | bitWidth n < 15 = toText131415 n
  | otherwise = fail "too long number"

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

-- | four to six digit number
toText456 n
  | n < 1000 = toText3 n
  | mod n 1000 == 0 = toText3 (div n 1000) ++ " thousand"
  | otherwise = toText456 (n - (mod n 1000)) ++ ", " ++ toText3 (mod n 1000)

-- | seven to nine digit number
toText789 n
  | n < 1000000 = toText456 n
  | mod n 1000000 == 0 = toText3 (div n 1000000) ++ " million"
  | otherwise = toText789 (n - (mod n 1000000)) ++ ", " ++ toText456 (mod n 1000000)

-- | ten to twelve digit number
toText101112 n
  | n < 1000000000 = toText789 n
  | mod n 1000000000 == 0 = toText3 (div n 1000000000) ++ " billion"
  | otherwise = toText101112 (n - (mod n 1000000000)) ++ ", " ++ toText789 (mod n 1000000000)

-- | thirteen to fifteen digit number
toText131415 n
  | n < 1000000000000 = toText101112 n
  | mod n 1000000000000 == 0 = toText3 (div n 1000000000000) ++ " trillion"
  | otherwise = toText131415 (n - (mod n 1000000000000)) ++ ", " ++ toText101112 (mod n 1000000000000)



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
