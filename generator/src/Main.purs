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

module Main where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION())
import Node.FS.Sync (readTextFile, writeTextFile)
import Node.FS (FS())
import Data.Foldable (foldMap)
import Data.Shortcut.Modifiers (KeyModifiers(..), CodeModifiers(..))
import Data.String (joinWith)
import Data.String.Capitalize (capitalize)
import Data.String.Multiline (multiline)
import Node.Encoding (Encoding(..))

data ShortcutSource
  = KeyShortcutSource { n :: String, k :: String, m :: Array KeyModifiers }
  | CodeShortcutSource { n :: String, c :: String, m :: Array CodeModifiers }

shortcutSources :: Array ShortcutSource
shortcutSources = keyShortcuts ++ codeShortcuts

codeShortcuts :: Array ShortcutSource
codeShortcuts =
  CodeShortcutSource <$>
    [ { n: "zero",      c: "Digit0",     m: [None, Alt, Shift, Mod, AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "one",       c: "Digit1",     m: [None, Alt, Shift,      AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "two",       c: "Digit2",     m: [None, Alt, Shift,      AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "three",     c: "Digit3",     m: [None, Alt, Shift,      AltShift, AltMod,           AltShiftMod] }
    , { n: "four",      c: "Digit4",     m: [None, Alt, Shift,      AltShift, AltMod,           AltShiftMod] }
    , { n: "five",      c: "Digit5",     m: [None, Alt, Shift,      AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "six",       c: "Digit6",     m: [None, Alt, Shift,      AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "seven",     c: "Digit7",     m: [None, Alt, Shift,      AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "eight",     c: "Digit8",     m: [None, Alt, Shift,      AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "nine",      c: "Digit9",     m: [None, Alt, Shift,      AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "space",     c: "Space",      m: [None, Alt, Shift,      AltShift,         ShiftMod, AltShiftMod] }
    , { n: "tab",       c: "Tab",        m: [None,      Shift,                AltMod,           AltShiftMod] }
    , { n: "enter",     c: "Enter",      m: [None, Alt, Shift, Mod, AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "home",      c: "Home",       m: [None, Alt, Shift, Mod, AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "end",       c: "End",        m: [None, Alt, Shift, Mod, AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "pageDown",  c: "PageDown",   m: [None, Alt, Shift, Mod, AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "pageUp",    c: "PageUp",     m: [None, Alt, Shift, Mod, AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "up",        c: "ArrowUp",    m: [None, Alt, Shift, Mod, AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "down",      c: "ArrowDown",  m: [None, Alt, Shift, Mod, AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "left",      c: "ArrowLeft",  m: [None, Alt, Shift, Mod, AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "right",     c: "ArrowRight", m: [None, Alt, Shift, Mod, AltShift, AltMod, ShiftMod, AltShiftMod] }
    , { n: "backspace", c: "Backspace",  m: [None, Alt, Shift, Mod, AltShift, AltMod, ShiftMod, AltShiftMod] }
    ]

keyShortcuts :: Array ShortcutSource
keyShortcuts =
  KeyShortcutSource <$>
    [ { n: "a", k: "a", m: [KeyNone, KeyMod] }
    , { n: "b", k: "b", m: [KeyNone, KeyMod] }
    , { n: "c", k: "c", m: [KeyNone, KeyMod] }
    , { n: "d", k: "d", m: [KeyNone, KeyMod] }
    , { n: "e", k: "e", m: [KeyNone, KeyMod] }
    , { n: "f", k: "f", m: [KeyNone, KeyMod] }
    , { n: "g", k: "g", m: [KeyNone, KeyMod] }
    , { n: "h", k: "h", m: [KeyNone, KeyMod] }
    , { n: "i", k: "i", m: [KeyNone, KeyMod] }
    , { n: "j", k: "j", m: [KeyNone, KeyMod] }
    , { n: "k", k: "k", m: [KeyNone, KeyMod] }
    , { n: "l", k: "l", m: [KeyNone, KeyMod] }
    , { n: "m", k: "m", m: [KeyNone] }
    , { n: "n", k: "n", m: [KeyNone, KeyMod] }
    , { n: "o", k: "o", m: [KeyNone, KeyMod] }
    , { n: "p", k: "p", m: [KeyNone, KeyMod] }
    , { n: "q", k: "q", m: [KeyNone] }
    , { n: "r", k: "r", m: [KeyNone, KeyMod] }
    , { n: "s", k: "s", m: [KeyNone, KeyMod] }
    , { n: "t", k: "t", m: [KeyNone, KeyMod] }
    , { n: "u", k: "u", m: [KeyNone, KeyMod] }
    , { n: "v", k: "v", m: [KeyNone, KeyMod] }
    , { n: "w", k: "w", m: [KeyNone] }
    , { n: "x", k: "x", m: [KeyNone, KeyMod] }
    , { n: "y", k: "y", m: [KeyNone, KeyMod] }
    , { n: "z", k: "z", m: [KeyNone, KeyMod] }
    , { n: "semicolon", k: ";", m: [KeyNone, KeyMod] }
    , { n: "colon", k: ":", m: [KeyNone, KeyMod] }
    , { n: "backtick", k: "`", m: [KeyNone, KeyMod] }
    , { n: "plus", k: "+", m: [KeyNone, KeyMod] }
    , { n: "minus", k: "-", m: [KeyNone, KeyMod] }
    , { n: "equals", k: "=", m: [KeyNone, KeyMod] }
    , { n: "underscore", k: "_", m: [KeyNone, KeyMod] }
    , { n: "leftBracket", k: "[", m: [KeyNone, KeyMod] }
    , { n: "rightBracket", k: "]", m: [KeyNone, KeyMod] }
    , { n: "leftCurlyBrace", k: "{", m: [KeyNone, KeyMod] }
    , { n: "rightCurlyBrace", k: "}", m: [KeyNone, KeyMod] }
    , { n: "forwardSlash", k: "/", m: [KeyNone, KeyMod] }
    , { n: "backSlash", k: "\\", m: [KeyNone, KeyMod] }
    , { n: "period", k: ".", m: [KeyNone, KeyMod] }
    , { n: "comma", k: ",", m: [KeyNone, KeyMod] }
    ]

generateKeyShortcutName :: String -> KeyModifiers -> String
generateKeyShortcutName name KeyNone = name
generateKeyShortcutName name KeyMod = "mod" ++ capitalize name

generateCodeShortcutName :: String -> CodeModifiers -> String
generateCodeShortcutName name None = name
generateCodeShortcutName name Alt = "alt" ++ capitalize name
generateCodeShortcutName name Shift = "shift" ++ capitalize name
generateCodeShortcutName name Mod = "mod" ++ capitalize name
generateCodeShortcutName name AltShift = "altShift" ++ capitalize name
generateCodeShortcutName name AltMod = "altMod" ++ capitalize name
generateCodeShortcutName name ShiftMod = "shiftMod" ++ capitalize name
generateCodeShortcutName name AltShiftMod = "altShiftMod" ++ capitalize name

generateKeyShortcut :: String -> KeyModifiers -> String
generateKeyShortcut key modifiers = "KeyShortcut " ++ show key ++ " " ++ show modifiers

generateCodeShortcut :: String -> CodeModifiers -> String
generateCodeShortcut code modifiers = "CodeShortcut " ++ show code ++ " " ++ show modifiers

generateDefinition :: String -> String -> String
generateDefinition name value =
  multiline
    [ name ++ " :: Shortcut"
    , name ++ " = " ++ value
    , ""
    ]

generateKeyShortcutDefinition :: String -> String -> KeyModifiers -> String
generateKeyShortcutDefinition sourceName key modifiers = generateDefinition name value
  where
  name = generateKeyShortcutName sourceName modifiers
  value = generateKeyShortcut key modifiers

generateCodeShortcutDefinition :: String -> String -> CodeModifiers -> String
generateCodeShortcutDefinition sourceName code modifiers = generateDefinition name value
  where
  name = generateCodeShortcutName sourceName modifiers
  value = generateCodeShortcut code modifiers

generateShortcuts :: ShortcutSource -> Array String
generateShortcuts (KeyShortcutSource source) =
  (generateKeyShortcutDefinition source.n source.k <$> source.m)
generateShortcuts (CodeShortcutSource source) =
  (generateCodeShortcutDefinition source.n source.c <$> source.m)

generateNames :: ShortcutSource -> Array String
generateNames (KeyShortcutSource source) = generateKeyShortcutName source.n <$> source.m
generateNames (CodeShortcutSource source) = generateCodeShortcutName source.n <$> source.m

generatedNames :: Array String
generatedNames = foldMap generateNames shortcutSources

partialNames :: Array String
partialNames =
  [ "Shortcut()"
  , "print"
  , "keyboardEventMatch"
  ]

generatedExports :: String
generatedExports = "  ( " ++ (joinWith "\n  , " (partialNames ++ generatedNames)) ++ "\n  ) where\n"

generatedDefinitions :: String
generatedDefinitions = multiline $ foldMap generateShortcuts shortcutSources

moduleHeader :: String
moduleHeader = "module Data.Shortcut\n"

comment :: String -> String
comment s = "{-\n" ++ s ++ "-}\n\n"

getLicence :: forall eff. Eff (err :: EXCEPTION, fs :: FS | eff) String
getLicence = comment <$> readTextFile UTF8 "licence.txt"

getPartial :: forall eff. Eff (err :: EXCEPTION, fs :: FS | eff) String
getPartial = readTextFile UTF8 "partial.purs"

generateModule :: String -> String -> String
generateModule licence partial =
  licence ++ moduleHeader ++ generatedExports ++ "\n" ++ partial ++ generatedDefinitions ++ "\n"

writeShortcutsModule :: forall eff. String -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
writeShortcutsModule = writeTextFile UTF8 "../src/Data/Shortcut.purs"

main :: Eff (err :: EXCEPTION, fs :: FS) Unit
main = generateModule <$> getLicence <*> getPartial >>= writeShortcutsModule

