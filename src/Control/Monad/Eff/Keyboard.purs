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

module Control.Monad.Eff.Keyboard where

import Prelude

import Control.Apply ((<*))
import Control.Bind ((<=<))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION(), error, throwException)
import Control.Monad.Eff.Ref (REF(), newRef, modifyRef, readRef, writeRef)

import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Foreign.Class (readProp)
import Data.NormalKey (NormalKey(), normalize)
import Data.Set (Set(), empty, insert)

import DOM as DOM
import DOM.Event.Event as DOM
import DOM.Event.EventTarget as DOM
import DOM.Event.EventTypes as DOM
import DOM.Event.Types as DOM

type KeyboardListeners eff = { keydown :: DOM.EventListener eff, keyup :: DOM.EventListener eff }

type KeyboardEffects eff = (ref :: REF, err :: EXCEPTION,  dom :: DOM.DOM | eff)

-- | Calls the provided function when the specified key combination is
-- | depressed. Returns an eff with a return value of a record which contains
-- | the event listeners. These can be used to undo any keyboard combination
-- | bindings created using this function.
-- |
-- | Relies on [DOM Level 3 `KeyboardEvent.key` values](http://www.w3.org/TR/DOM-Level-3-Events-key/),
-- | if your target browser does not support these then consider using a
-- | polyfill such as the one provided by [`js-polyfills`](https://github.com/inexorabletash/polyfill/blob/master/keyboard.md).
-- |
-- | It is recommended that "Alt" is avoided as this changes the input
-- | characters on some platforms.

onKeyCombination :: forall eff. DOM.EventTarget -> ((Set NormalKey) -> Eff (KeyboardEffects eff) Unit) -> Set NormalKey -> Eff (KeyboardEffects eff) (KeyboardListeners (KeyboardEffects eff))
onKeyCombination target callback expectedCombination = do
  currentCombination <- newRef empty
  let clearCombination = writeRef currentCombination empty
  let insertListener = DOM.eventListener $ insertKeyAndCallbackIfExpected currentCombination
  let clearListener = DOM.eventListener $ const clearCombination
  DOM.addEventListener DOM.keydown insertListener false target
  DOM.addEventListener DOM.keyup clearListener false target
  pure { keydown: insertListener, keyup: clearListener }

  where
  pluckNormalKey = pure <<< normalize <=< readProp "key" <<< toForeign

  refInsert ref = const (readRef ref) <=< modifyRef ref <<< insert

  insertKeyAndCallbackIfExpected ref e =
    either
      (const (throwException $ error "Couldn't read `key` property of KeyboardEvent."))
      (callbackIfExpected e <=< refInsert ref)
      (pluckNormalKey e)

  callbackIfExpected e combination =
    if combination == expectedCombination
      then callback combination <* DOM.preventDefault e
      else pure unit
