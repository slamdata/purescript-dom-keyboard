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

module Data.ModifierKey where

import Prelude

import Data.Char (fromCharCode)
import Data.String (fromChar)

data ModifierKey = Shift | Control | Alt | Meta

instance eqModifierKey :: Eq ModifierKey where
  eq x y = compare x y == EQ

instance ordModifierKey :: Ord ModifierKey where
  compare x y = compare (toView x) (toView y)

instance showModifierKey :: Show ModifierKey where
  show Control = "Control"
  show Shift = "Shift"
  show Alt = "Alt"
  show Meta = "Meta"

toApple :: ModifierKey -> ModifierKey
toApple Control = Meta
toApple modifier = modifier

toOther :: ModifierKey -> ModifierKey
toOther Meta = Control
toOther modifier = modifier

toView :: ModifierKey -> Int
toView Shift = 0
toView Control = 1
toView Alt = 2
toView Meta = 3

print :: ModifierKey -> String
print Control = "Ctrl"
print modifierKey = show modifierKey

printApple :: ModifierKey -> String
printApple Control = fromChar $ fromCharCode 8963
printApple Meta = fromChar $ fromCharCode 8984
printApple Alt = fromChar $ fromCharCode 8997
printApple Shift = fromChar $ fromCharCode 8679

