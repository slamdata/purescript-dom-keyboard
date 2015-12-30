module Data.String.Capitalize where

import Prelude
import Data.String (toUpper, take, drop)

capitalize :: String -> String
capitalize s = (toUpper $ take 1 s) <> (drop 1 s)

