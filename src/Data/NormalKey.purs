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

module Data.NormalKey
  ( NormalKey()
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

separator :: String
separator = "+"

separatorApple :: String
separatorApple = ""

printCombination' :: String -> (NormalKey -> String) -> Set NormalKey -> String
printCombination' s f = toList >>> map f >>> intercalate s

printCombination :: Set NormalKey -> String
printCombination = printCombination' separator print

printCombinationApple :: Set NormalKey -> String
printCombinationApple = printCombination' separatorApple printApple

normalize :: String -> NormalKey
normalize s | length s == 1 = NormalKey $ toUpper s
normalize s = NormalKey s

normalizeCombination :: Array String -> Set NormalKey
normalizeCombination = foldMap (normalize >>> singleton)

toApple :: NormalKey -> NormalKey
toApple (NormalKey "Control") = NormalKey "Meta"
toApple key = key

combinationToApple :: Set NormalKey -> Set NormalKey
combinationToApple = toList >>> map toApple >>> fromList

data NormalKeyView = Modifier Int | Other String | Enter

toView :: NormalKey -> NormalKeyView
toView (NormalKey "Control") = Modifier 0
toView (NormalKey "Meta") = Modifier 1
toView (NormalKey "Alt") = Modifier 2
toView (NormalKey "Shift") = Modifier 3
toView (NormalKey "Enter") = Enter
toView (NormalKey s) = Other s

instance eqNormalKey :: Eq NormalKey where
  eq (NormalKey x) (NormalKey y) = eq x y

instance eqNormalKeyView :: Eq NormalKeyView where
  eq x y = compare x y == EQ

instance ordNormalKeyView :: Ord NormalKeyView where
  compare (Modifier x) (Modifier y) = compare x y
  compare (Modifier _) _ = LT
  compare _ (Modifier _) = GT
  compare (Other x) (Other y) = compare x y
  compare Enter Enter = EQ
  compare Enter _ = GT
  compare _ Enter = LT

instance ordNormalKey :: Ord NormalKey where
  compare x y = compare (toView x) (toView y)

instance showNormalKey :: Show NormalKey where
  show (NormalKey s) = "(NormalKey " ++ show s ++ ")"

