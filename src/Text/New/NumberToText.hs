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
  | bitWidth n < 7 = toText6 n
  | bitWidth n < 10 = toText9 n
  | bitWidth n < 12 = toText12 n
  | bitWidth n < 15 = toText15 n
  | bitWidth n < 18 = toText18 n
  | bitWidth n < 21 = toText21 n
  | bitWidth n < 24 = toText24 n
  | bitWidth n < 27 = toText27 n
  | bitWidth n < 30 = toText30 n
  | bitWidth n < 33 = toText33 n
  | bitWidth n < 36 = toText36 n
  | bitWidth n < 39 = toText39 n
  | bitWidth n < 42 = toText42 n
  | bitWidth n < 45 = toText45 n
  | bitWidth n < 48 = toText48 n
  | bitWidth n < 51 = toText51 n
  | bitWidth n < 54 = toText54 n
  | bitWidth n < 57 = toText57 n
  | bitWidth n < 60 = toText60 n
  | bitWidth n < 63 = toText63 n
  | bitWidth n < 66 = toText66 n
  | n == (10^100) = "one googol"
  | n == (10^303) = "one centillion"
  | n == (10^3003) = "one millillion"
  | n == (10^3000003) = "one milli-millillion"
  | n == (10^(10^100)) = "one googolplex"
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

-- | 4 to 6 digit number
toText6 n
  | n < 10^3 = toText3 n
  | (mod n $ 10^3) == 0 = toText3 (div n $ 10^3) ++ " thousand"
  | otherwise = toText6 (n - (mod n $ 10^3)) ++ ", " ++ toText3 (mod n $ 10^3)

-- | 7 to 9 digit number
toText9 n
  | n < 10^6 = toText6 n
  | (mod n $ 10^6) == 0 = toText3 (div n $ 10^6) ++ " million"
  | otherwise = toText9 (n - (mod n $ 10^6)) ++ ", " ++ toText6 (mod n $ 10^6)

-- | 10 to 12 digit number
toText12 n
  | n < 10^9 = toText9 n
  | (mod n $ 10^9) == 0 = toText3 (div n $ 10^9) ++ " billion"
  | otherwise = toText12 (n - (mod n $ 10^9)) ++ ", " ++ toText9 (mod n $ 10^9)

-- | 13 to 15 digit number
toText15 n
  | n < 10^12 = toText12 n
  | (mod n $ 10^12) == 0 = toText3 (div n $ 10^12) ++ " trillion"
  | otherwise = toText15 (n - (mod n $ 10^12)) ++ ", " ++ toText12 (mod n $ 10^12)

-- | 16 to 18 digit number
toText18 n
  | n < 10^15 = toText15 n
  | (mod n $ 10^15) == 0 = toText3 (div n $ 10^15) ++ " quadrillion"
  | otherwise = toText18 (n - (mod n $ 10^15)) ++ ", " ++ toText15 (mod n $ 10^15)

-- | 19 to 21 digit number
toText21 n
  | n < 10^18 = toText18 n
  | (mod n $ 10^18) == 0 = toText3 (div n $ 10^18) ++ " quintillion"
  | otherwise = toText21 (n - (mod n $ 10^18)) ++ ", " ++ toText18 (mod n $ 10^18)

-- | 22 to 24 digit number
toText24 n
  | n < 10^21 = toText21 n
  | (mod n $ 10^21) == 0 = toText3 (div n $ 10^21) ++ " sextillion"
  | otherwise = toText24 (n - (mod n $ 10^21)) ++ ", " ++ toText21 (mod n $ 10^21)

-- | 25 to 27 digit number
toText27 n
  | n < 10^24 = toText24 n
  | (mod n $ 10^24) == 0 = toText3 (div n $ 10^24) ++ " septillion"
  | otherwise = toText27 (n - (mod n $ 10^24)) ++ ", " ++ toText24 (mod n $ 10^24)

-- | 28 to 30 digit number
toText30 n
  | n < 10^27 = toText27 n
  | (mod n $ 10^27) == 0 = toText3 (div n $ 10^27) ++ " octillion"
  | otherwise = toText30 (n - (mod n $ 10^27)) ++ ", " ++ toText27 (mod n $ 10^27)

-- | 31 to 33 digit number
toText33 n
  | n < 10^30 = toText30 n
  | (mod n $ 10^30) == 0 = toText3 (div n $ 10^30) ++ " nonillion"
  | otherwise = toText33 (n - (mod n $ 10^30)) ++ ", " ++ toText30 (mod n $ 10^30)

-- | 34 to 36 digit number
toText36 n
  | n < 10^33 = toText33 n
  | (mod n $ 10^33) == 0 = toText3 (div n $ 10^33) ++ " decillion"
  | otherwise = toText36 (n - (mod n $ 10^33)) ++ ", " ++ toText33 (mod n $ 10^33)

-- | 37 to 39 digit number
toText39 n
  | n < 10^36 = toText36 n
  | (mod n $ 10^36) == 0 = toText3 (div n $ 10^36) ++ " undecillion"
  | otherwise = toText39 (n - (mod n $ 10^36)) ++ ", " ++ toText36 (mod n $ 10^36)

-- | 40 to 42 digit number
toText42 n
  | n < 10^39 = toText39 n
  | (mod n $ 10^39) == 0 = toText3 (div n $ 10^39) ++ " duodecillion"
  | otherwise = toText42 (n - (mod n $ 10^39)) ++ ", " ++ toText39 (mod n $ 10^39)

-- | 43 to 45 digit number
toText45 n
  | n < 10^42 = toText42 n
  | (mod n $ 10^42) == 0 = toText3 (div n $ 10^42) ++ " tredecillion"
  | otherwise = toText45 (n - (mod n $ 10^42)) ++ ", " ++ toText42 (mod n $ 10^42)

-- | 46 to 48 digit number
toText48 n
  | n < 10^45 = toText45 n
  | (mod n $ 10^45) == 0 = toText3 (div n $ 10^45) ++ " quattuordecillion"
  | otherwise = toText48 (n - (mod n $ 10^45)) ++ ", " ++ toText45 (mod n $ 10^45)

-- | 49 to 51 digit number
toText51 n
  | n < 10^48 = toText48 n
  | (mod n $ 10^48) == 0 = toText3 (div n $ 10^48) ++ " quinquadecillion"
  | otherwise = toText51 (n - (mod n $ 10^48)) ++ ", " ++ toText48 (mod n $ 10^48)

-- | 52 to 54 digit number
toText54 n
  | n < 10^51 = toText51 n
  | (mod n $ 10^51) == 0 = toText3 (div n $ 10^51) ++ " sexdecillion"
  | otherwise = toText54 (n - (mod n $ 10^51)) ++ ", " ++ toText51 (mod n $ 10^51)

-- | 55 to 57 digit number
toText57 n
  | n < 10^54 = toText54 n
  | (mod n $ 10^54) == 0 = toText3 (div n $ 10^54) ++ " septendecillion"
  | otherwise = toText57 (n - (mod n $ 10^54)) ++ ", " ++ toText54 (mod n $ 10^54)

-- | 58 to 60 digit number
toText60 n
  | n < 10^57 = toText57 n
  | (mod n $ 10^57) == 0 = toText3 (div n $ 10^57) ++ " octodecillion"
  | otherwise = toText60 (n - (mod n $ 10^57)) ++ ", " ++ toText57 (mod n $ 10^57)

-- | 61 to 63 digit number
toText63 n
  | n < 10^60 = toText60 n
  | (mod n $ 10^60) == 0 = toText3 (div n $ 10^60) ++ " novendecillion"
  | otherwise = toText63 (n - (mod n $ 10^60)) ++ ", " ++ toText60 (mod n $ 10^60)

-- | 64 to 66 digit number
toText66 n
  | n < 10^63 = toText63 n
  | (mod n $ 10^63) == 0 = toText3 (div n $ 10^63) ++ " vigintillion"
  | otherwise = toText66 (n - (mod n $ 10^63)) ++ ", " ++ toText63 (mod n $ 10^63)

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
