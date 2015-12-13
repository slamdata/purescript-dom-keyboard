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
import Control.Apply ((*>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (EXCEPTION(), error, throwException)

import Data.Either (either)
import Data.KeyboardEvent (eqKeyboardEvent, readKeyboardEvent)
import Data.KeyCombination (KeyCombination(), toKeyboardEvent)

import DOM as DOM
import DOM.Event.Event as DOM
import DOM.Event.EventTarget as DOM
import DOM.Event.EventTypes as DOM
import DOM.Event.Types as DOM

type KeyboardEffects eff = (err :: EXCEPTION,  dom :: DOM.DOM | eff)

-- | Performs the provided effect when the specified key combination is
-- | depressed. Returns an eff with a return value of an event listener. This
-- | can be used to undo any bindings created using this function.
-- |
-- | This function only supports combinations containing a single non-modifier
-- | key. This is due to issues with the `Command` key on Apple platforms.
-- |
-- | Relies on [DOM Level 3 `KeyboardEvent.code` values](http://www.w3.org/TR/DOM-Level-3-Events-code/),
-- | if your target browser does not support these then consider using a
-- | polyfill such as the one provided by [`js-polyfills`](https://github.com/inexorabletash/polyfill/blob/master/keyboard.md).

onKeyCombination :: forall eff. DOM.EventTarget -> Eff (KeyboardEffects eff) Unit -> KeyCombination -> Eff (KeyboardEffects eff) (DOM.EventListener (KeyboardEffects eff))
onKeyCombination target action expectedKeyCombination =
  DOM.addEventListener DOM.keydown listener false target *> pure listener

  where
  expectedKeyboardEvent = toKeyboardEvent expectedKeyCombination
  errorMessage = "Couldn't read KeyboardEvent. Does your browser support DOM Level 3 KeyboardEvent code values?"
  throw = const (throwException $ error errorMessage)
  actIfExpected e keyboardEvent =
    if keyboardEvent `eqKeyboardEvent` expectedKeyboardEvent
       then DOM.preventDefault e *> action
       else pure unit
  readAndActIfExpected e = either throw (actIfExpected e) $ readKeyboardEvent e
  listener = DOM.eventListener $ readAndActIfExpected

