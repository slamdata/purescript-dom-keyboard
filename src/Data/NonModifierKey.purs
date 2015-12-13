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

module Data.NonModifierKey where

import Prelude

import Data.Char (fromCharCode)
import Data.Generic (Generic, gEq, gCompare)
import Data.Maybe (fromMaybe)
import Data.String (fromChar, stripPrefix)
import Data.Foldable (foldr)

data NonModifierKey
  = Enter
  | Space
  | Tab
  | Backquote
  | Backslash
  | Backspace
  | BracketLeft
  | BracketRight
  | Comma
  | Digit0
  | Digit1
  | Digit2
  | Digit3
  | Digit4
  | Digit5
  | Digit6
  | Digit7
  | Digit8
  | Digit9
  | Equal
  | IntlBackslash
  | IntlHash
  | IntlRo
  | IntlYen
  | KeyA
  | KeyB
  | KeyC
  | KeyD
  | KeyE
  | KeyF
  | KeyG
  | KeyH
  | KeyI
  | KeyJ
  | KeyK
  | KeyL
  | KeyM
  | KeyN
  | KeyO
  | KeyP
  | KeyQ
  | KeyR
  | KeyS
  | KeyT
  | KeyU
  | KeyV
  | KeyW
  | KeyX
  | KeyY
  | KeyZ
  | Minus
  | Period
  | Quote
  | Semicolon
  | Slash
  | Delete
  | End
  | Help
  | Home
  | PageDown
  | PageUp
  | ArrowDown
  | ArrowLeft
  | ArrowRight
  | ArrowUp
  | Escape
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12

derive instance genericNonModifierKey :: Generic NonModifierKey

instance showNonModifierKey :: Show NonModifierKey where
  show Enter = "Enter"
  show Space = "Space"
  show Tab = "Tab"
  show Backquote = "Backquote"
  show Backslash = "Backslash"
  show Backspace = "Backspace"
  show BracketLeft = "BracketLeft"
  show BracketRight = "BracketRight"
  show Comma = "Comma"
  show Digit0 = "Digit0"
  show Digit1 = "Digit1"
  show Digit2 = "Digit2"
  show Digit3 = "Digit3"
  show Digit4 = "Digit4"
  show Digit5 = "Digit5"
  show Digit6 = "Digit6"
  show Digit7 = "Digit7"
  show Digit8 = "Digit8"
  show Digit9 = "Digit9"
  show Equal = "Equal"
  show IntlBackslash = "IntlBackslash"
  show IntlHash = "IntlHash"
  show IntlRo = "IntlRo"
  show IntlYen = "IntlYen"
  show KeyA = "KeyA"
  show KeyB = "KeyB"
  show KeyC = "KeyC"
  show KeyD = "KeyD"
  show KeyE = "KeyE"
  show KeyF = "KeyF"
  show KeyG = "KeyG"
  show KeyH = "KeyH"
  show KeyI = "KeyI"
  show KeyJ = "KeyJ"
  show KeyK = "KeyK"
  show KeyL = "KeyL"
  show KeyM = "KeyM"
  show KeyN = "KeyN"
  show KeyO = "KeyO"
  show KeyP = "KeyP"
  show KeyQ = "KeyQ"
  show KeyR = "KeyR"
  show KeyS = "KeyS"
  show KeyT = "KeyT"
  show KeyU = "KeyU"
  show KeyV = "KeyV"
  show KeyW = "KeyW"
  show KeyX = "KeyX"
  show KeyY = "KeyY"
  show KeyZ = "KeyZ"
  show Minus = "Minus"
  show Period = "Period"
  show Quote = "Quote"
  show Semicolon = "Semicolon"
  show Slash = "Slash"
  show Delete = "Delete"
  show End = "End"
  show Help = "Help"
  show Home = "Home"
  show PageDown = "PageDown"
  show PageUp = "PageUp"
  show ArrowDown = "ArrowDown"
  show ArrowLeft = "ArrowLeft"
  show ArrowRight = "ArrowRight"
  show ArrowUp = "ArrowUp"
  show Escape = "Escape"
  show F1 = "F1"
  show F2 = "F2"
  show F3 = "F3"
  show F4 = "F4"
  show F5 = "F5"
  show F6 = "F6"
  show F7 = "F7"
  show F8 = "F8"
  show F9 = "F9"
  show F10 = "F10"
  show F11 = "F11"
  show F12 = "F12"

instance eqNonModifierKey :: Eq NonModifierKey where eq = gEq

instance ordNonModifierKey :: Ord NonModifierKey where compare = gCompare

print :: NonModifierKey -> String
print key = stripPrefixes ["Key", "Arrow", "Digit"] $ show key

printApple :: NonModifierKey -> String
printApple Tab = fromChar $ fromCharCode 8677
printApple End = fromChar $ fromCharCode 8600
printApple Home = fromChar $ fromCharCode 8598
printApple PageDown = fromChar $ fromCharCode 10504
printApple PageUp = fromChar $ fromCharCode 10505
printApple ArrowDown = fromChar $ fromCharCode 8595
printApple ArrowLeft = fromChar $ fromCharCode 8592
printApple ArrowRight = fromChar $ fromCharCode 8594
printApple ArrowUp = fromChar $ fromCharCode 8593
printApple key = print key

stripPrefix' :: String -> String -> String
stripPrefix' prefix s = fromMaybe s $ stripPrefix prefix s

stripPrefixes :: Array String -> String -> String
stripPrefixes prefixes string = foldr stripPrefix' string prefixes

toCode :: NonModifierKey -> String
toCode = show

