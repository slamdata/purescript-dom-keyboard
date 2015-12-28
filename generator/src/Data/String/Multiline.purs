module Data.String.Multiline where

import Data.String (joinWith)

multiline :: Array String -> String
multiline = joinWith "\n"

