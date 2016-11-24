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

module Data.Shortcut.Modifiers where

import Prelude
import Data.Char (fromCharCode)
import Data.KeyboardEvent (KeyboardEvent())
import Data.Shortcut.Platform (Platform(..))
import Data.String (joinWith)

data KeyModifiers = KeyNone | KeyMod

data CodeModifiers = None | Alt | Shift | Mod | AltShift | AltMod | ShiftMod | AltShiftMod

separator :: Platform -> String
separator Other = "+"
separator Apple = ""

printCMOther :: CodeModifiers -> String
printCMOther None = ""
printCMOther Alt = "Alt"
printCMOther Shift = "Shift"
printCMOther Mod = "Ctrl"
printCMOther AltShift = joinWith (separator Other) $ printCMOther <$> [Alt, Shift]
printCMOther AltMod = joinWith (separator Other) $ printCMOther <$> [Alt, Mod]
printCMOther ShiftMod = joinWith (separator Other) $ printCMOther <$> [Shift, Mod]
printCMOther AltShiftMod = joinWith (separator Other) $ printCMOther <$> [Alt, Shift, Mod]

printCMApple :: CodeModifiers -> String
printCMApple None = ""
printCMApple Alt = show $ fromCharCode 8997
printCMApple Shift = show $ fromCharCode 8679
printCMApple Mod = show $ fromCharCode 8984
printCMApple AltShift = joinWith (separator Apple) $ printCMApple <$> [Alt, Shift]
printCMApple AltMod = joinWith (separator Apple) $ printCMApple <$> [Alt, Mod]
printCMApple ShiftMod = joinWith (separator Apple) $ printCMApple <$> [Shift, Mod]
printCMApple AltShiftMod = joinWith (separator Apple) $ printCMApple <$> [Alt, Shift, Mod]

printCodeModifiers :: Platform -> CodeModifiers -> String
printCodeModifiers Other = printCMOther
printCodeModifiers Apple = printCMApple

matchCM :: (KeyboardEvent -> Boolean) -> KeyboardEvent -> CodeModifiers -> Boolean
matchCM modKey e None = e.altKey == false && e.shiftKey == false && (modKey e) == false
matchCM modKey e Alt = e.altKey == true && e.shiftKey == false && (modKey e) == false
matchCM modKey e Shift = e.altKey == false && e.shiftKey == true && (modKey e) == false
matchCM modKey e Mod = e.altKey == false && e.shiftKey == false && (modKey e) == true
matchCM modKey e AltShift = e.altKey == true && e.shiftKey == true && (modKey e) == false
matchCM modKey e AltMod = e.altKey == true && e.shiftKey == false && (modKey e) == true
matchCM modKey e ShiftMod = e.altKey == false && e.shiftKey == true && (modKey e) == true
matchCM modKey e AltShiftMod = e.altKey == true && e.shiftKey == true && (modKey e) == true

matchCodeModifiers :: Platform -> KeyboardEvent -> CodeModifiers -> Boolean
matchCodeModifiers Other = matchCM _.ctrlKey
matchCodeModifiers Apple = matchCM _.metaKey

printKeyModifiers :: Platform -> KeyModifiers -> String
printKeyModifiers Other KeyNone = printCMOther None
printKeyModifiers Other KeyMod = printCMOther Mod
printKeyModifiers Apple KeyNone = printCMApple None
printKeyModifiers Apple KeyMod = printCMApple Mod

matchKeyModifiers :: Platform -> KeyboardEvent -> KeyModifiers -> Boolean
matchKeyModifiers Other e KeyNone = e.ctrlKey == false
matchKeyModifiers Other e KeyMod = e.ctrlKey == true
matchKeyModifiers Apple e KeyNone = e.metaKey == false
matchKeyModifiers Apple e KeyMod = e.metaKey == true

instance showCodeModifiers :: Show CodeModifiers where
  show None = "None"
  show Alt = "Alt"
  show Shift = "Shift"
  show Mod = "Mod"
  show AltShift = "AltShift"
  show AltMod = "AltMod"
  show ShiftMod = "ShiftMod"
  show AltShiftMod = "AltShiftMod"

instance showKeyModifiers :: Show KeyModifiers where
  show KeyNone = "KeyNone"
  show KeyMod = "KeyMod"

