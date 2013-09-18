A program converts a number to a String
=======================================

Text.New.Speak.hs
-----------------
say :: String -> IO System.Process.Internals.ProcessHandle
>say "hello world"


Text.New.NumberToText
---------------------
numberToText :: Integer -> String
>numberToText 123

>"one hundred and twenty-three"

digitToText :: String -> String
>digitToText "1.2312341231231232132131232132"

>"one point two three one two three four one two three one two three one two three two one three two one three one two three two one three two"

