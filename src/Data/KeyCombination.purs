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

module Data.KeyCombination where

import Prelude

import Data.Array.Contains (contains)
import Data.Array (nub, sort)
import Data.String (joinWith)
import Data.ModifierKey as MK
import Data.NonModifierKey as K
import Data.KeyboardEvent (KeyboardEvent())
import Data.KeyPlatform (KeyPlatform(..))

type KeyCombination = { modifiers :: Array MK.ModifierKey, key :: K.NonModifierKey }

toApple :: KeyCombination -> KeyCombination
toApple combination =
 if combination.modifiers `contains` MK.Meta
   then combination
   else combination { modifiers = map MK.toApple combination.modifiers }

toOther :: KeyCombination -> KeyCombination
toOther combination =
 if combination.modifiers `contains` MK. Control
   then combination
   else combination { modifiers = map MK.toOther combination.modifiers }

separator :: String
separator = "+"

separatorApple :: String
separatorApple = ""

printModifiers' :: String -> (MK.ModifierKey -> String) -> Array MK.ModifierKey -> String
printModifiers' s f = sort >>> nub >>> map f >>> joinWith s

printModifiers :: Array MK.ModifierKey -> String
printModifiers = printModifiers' separator MK.print

printModifiersApple :: Array  MK.ModifierKey -> String
printModifiersApple = printModifiers' separatorApple MK.printApple

print :: KeyCombination -> String
print c = printModifiers c.modifiers ++ separator ++ K.print c.key

printApple :: KeyCombination -> String
printApple c = printModifiersApple c.modifiers ++ separatorApple ++ K.printApple c.key

printPlatform :: KeyPlatform -> KeyCombination -> String
printPlatform Other = print
printPlatform Apple = printApple

platformize :: KeyPlatform -> KeyCombination -> KeyCombination
platformize Other = toOther
platformize Apple = toApple

toKeyboardEvent :: KeyCombination -> KeyboardEvent
toKeyboardEvent c =
  { altKey: c.modifiers `contains` MK.Alt
  , ctrlKey: c.modifiers `contains` MK.Control
  , shiftKey: c.modifiers `contains` MK.Shift
  , metaKey: c.modifiers `contains` MK.Meta
  , code: K.toCode c.key
  }

