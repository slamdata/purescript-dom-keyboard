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

module Data.KeyboardEvent where

import Prelude

import Data.Foreign (Foreign(), F(), toForeign)
import Data.Foreign.Class (readProp)

type KeyboardEvent =
  { altKey :: Boolean
  , ctrlKey :: Boolean
  , shiftKey :: Boolean
  , metaKey :: Boolean
  , code :: String
  }

readKeyboardEvent' :: Foreign -> F KeyboardEvent
readKeyboardEvent' value =
  { altKey: _, ctrlKey: _, shiftKey: _, metaKey: _, code: _ }
    <$> readProp "altKey" value
    <*> readProp "ctrlKey" value
    <*> readProp "shiftKey" value
    <*> readProp "metaKey" value
    <*> readProp "code" value

readKeyboardEvent :: forall a. a -> F KeyboardEvent
readKeyboardEvent = readKeyboardEvent' <<< toForeign

eqKeyboardEvent :: KeyboardEvent -> KeyboardEvent -> Boolean
eqKeyboardEvent x y = eqAlt && eqControl && eqShift && eqMeta && eqCode
  where
  eqAlt = x.altKey == y.altKey
  eqControl = x.ctrlKey == y.ctrlKey
  eqShift = x.shiftKey == y.shiftKey
  eqMeta = x.metaKey == y.metaKey
  eqCode = x.code == y.code

