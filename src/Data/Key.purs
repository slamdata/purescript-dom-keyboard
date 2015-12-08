{-
Copyright 2015 SlamData, Inc.
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Data.Key
  ( Key(..)
  , NormalKey()
  , print
  , printApple
  , printCombination
  , printCombinationApple
  , normalize
  , normalizeCombination
  , toApple
  , combinationToApple
  ) where

import Prelude

import Data.Char (fromCharCode)
import Data.Foldable (intercalate, foldMap)
import Data.Set (Set(), fromList, toList, singleton)
import Data.String (fromChar, length, toUpper)

newtype Key = Key String
newtype NormalKey = NormalKey String

print :: NormalKey -> String
print (NormalKey "Control") = "Ctrl"
print (NormalKey s) = s

printApple :: NormalKey -> String
printApple (NormalKey "Control") = fromChar $ fromCharCode 8963
printApple (NormalKey "Meta") = fromChar $ fromCharCode 8984
printApple (NormalKey "Alt") = fromChar $ fromCharCode 8997
printApple (NormalKey "Shift") = fromChar $ fromCharCode 8679
printApple (NormalKey "Tab") = fromChar $ fromCharCode 8677
printApple (NormalKey "PageUp") = fromChar $ fromCharCode 10505
printApple (NormalKey "PageDown") = fromChar $ fromCharCode 10504
printApple (NormalKey "Home") = fromChar $ fromCharCode 8598
printApple (NormalKey "End") = fromChar $ fromCharCode 8600
printApple (NormalKey s) = s

seperator :: String
seperator = "+"

seperatorApple :: String
seperatorApple = ""

printCombination' :: String -> (NormalKey -> String) -> Set NormalKey -> String
printCombination' s f = toList >>> map f >>> intercalate s

printCombination :: Set NormalKey -> String
printCombination = printCombination' seperator print

printCombinationApple :: Set NormalKey -> String
printCombinationApple = printCombination' seperatorApple printApple

normalize :: Key -> NormalKey
normalize (Key s) | length s == 1 = NormalKey $ toUpper s
normalize (Key s) = NormalKey s

normalizeCombination :: Array Key -> Set NormalKey
normalizeCombination = foldMap (normalize >>> singleton)

toApple :: NormalKey -> NormalKey
toApple (NormalKey "Ctrl") = NormalKey "Meta"
toApple key = key

combinationToApple :: Set NormalKey -> Set NormalKey
combinationToApple = toList >>> map toApple >>> fromList

instance showKey :: Show Key where
  show (Key s) = "(Key " ++ show s ++ ")"

instance eqNormalKey :: Eq NormalKey where
  eq (NormalKey x) (NormalKey y) = eq x y

instance ordNormalKey :: Ord NormalKey where
  compare (NormalKey "Control") (NormalKey "Control") = EQ
  compare (NormalKey "Control") _ = LT
  compare (NormalKey "Meta") (NormalKey "Meta") = EQ
  compare (NormalKey "Meta") _ = LT
  compare _ (NormalKey "Meta") = GT
  compare (NormalKey "Alt") (NormalKey "Alt") = EQ
  compare (NormalKey "Alt") _ = LT
  compare _ (NormalKey "Alt") = GT
  compare (NormalKey "Shift") (NormalKey "Shift") = EQ
  compare (NormalKey "Shift") _ = LT
  compare _ (NormalKey "Shift") = GT
  compare (NormalKey "Enter") (NormalKey "Enter") = EQ
  compare (NormalKey x) (NormalKey y) = compare x y

instance showNormalKey :: Show NormalKey where
  show (NormalKey s) = "(NormalKey " ++ show s ++ ")"

