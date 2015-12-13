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
import Control.Monad.Eff.KeyPlatform (keyPlatform)
import Control.Monad.Eff.Keyboard (onKeyCombination)
import DOM (DOM())
import DOM.Event.EventTarget (removeEventListener)
import DOM.Event.EventTypes (keydown)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Types (documentToEventTarget)
import Data.KeyCombination (KeyCombination(), platformize, printPlatform)
import Data.ModifierKey (ModifierKey(..))
import Data.NonModifierKey (NonModifierKey(..))
import Util (log, onLoad)

helloCombination :: KeyCombination
helloCombination = { modifiers: [Control, Shift, Alt], key: KeyB }

howAreYouCombination :: KeyCombination
howAreYouCombination = { modifiers: [Control], key: KeyP }

goodbyeCombination :: KeyCombination
goodbyeCombination = { modifiers: [Control, Shift], key: KeyA }

main :: Eff (dom :: DOM, err :: EXCEPTION) Unit
main = onLoad \_ -> do
  -- Find out what platform you are on
  platform <- keyPlatform

  -- Tailor your combinations to this platform
  let platformizedHelloCombination = platformize platform helloCombination
  let platformizedHowAreYouCombination = platformize platform howAreYouCombination
  let platformizedGoodbyeCombination = platformize platform goodbyeCombination

  -- Retreive an `EventTarget` using purescript-dom
  target <- map (htmlDocumentToDocument >>> documentToEventTarget) (window >>= document)

  -- Bind your actions to key combinations, making references to the returned
  -- event listeners if you wish to unbind later.
  onKeyCombination target (log "Hello!") platformizedHelloCombination
  onKeyCombination target (log "How are you?") platformizedHowAreYouCombination
  listener <- onKeyCombination target (log "Goodbye!") platformizedGoodbyeCombination

  -- Unbind event listeners when you are done with them.
  removeEventListener keydown listener false target

  -- Use strings to present your combinations as appropriate for the given platform
  let helloCombinationString = printPlatform platform platformizedHelloCombination
  let howAreYouCombinationString = printPlatform platform platformizedHowAreYouCombination
  let goodbyeCombinationString = printPlatform platform platformizedGoodbyeCombination

  log $ helloCombinationString ++ ": appends \"Hello!\"."
  log $ howAreYouCombinationString ++ ": appends \"How are you?\"."
  log $ goodbyeCombinationString ++ ": would append \"Goodbye!\" but it has been unbound."

