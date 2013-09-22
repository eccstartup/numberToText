A program converts a number to a String
=======================================

Text.New.Speak.hs
-----------------
say :: String -> IO System.Process.Internals.ProcessHandle
>say "hello world"

NOTE:it works for mac os x


Text.New.NumberToText.hs
---------------------
numberToText :: Integer -> String
>numberToText 123

>"one hundred and twenty-three"

digitToText :: String -> String
>digitToText "1.2312341231231232132131232132"

>"one point two three one two three four one two three one two three one two three two one three two one three one two three two one three two"

[This is what I refer to.](http://www.mrob.com/pub/math/largenum.html)

--------------
[A new and detailed link](http://www.isthe.com/chongo/tech/math/number/howhigh.html)

--------------
TODO:
I don't know how big number I can manipulate.
There is some problem when it reaches "10^3006".
