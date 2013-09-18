module Text.New.Speak
       (
        say
       ) where

import System.Process

say s = do
  runCommand $ "say" ++ s
