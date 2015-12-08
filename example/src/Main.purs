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
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Keyboard (onKeyCombination)
import Control.Monad.Eff.Ref (REF())

import Data.Key (Key(..), NormalKey(), printCombinationApple, normalizeCombination)
import Data.Set (Set())

import DOM (DOM())
import DOM.Event.EventTarget (removeEventListener)
import DOM.Event.EventTypes (keyup, keydown)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Types (documentToEventTarget)

combination1 :: Set NormalKey
combination1 = normalizeCombination [Key "Alt", Key "Backspace"]

combination2 :: Set NormalKey
combination2 = normalizeCombination [Key "Meta", Key "Shift", Key "A"]

main :: Eff (console :: CONSOLE, ref :: REF, dom :: DOM, err :: EXCEPTION) Unit
main = do
  -- Retreive an `EventTarget` using purescript-dom
  doc <- map (htmlDocumentToDocument >>> documentToEventTarget) (window >>= document)

  onKeyCombination doc (printCombinationApple >>> log) combination1
  listeners <- onKeyCombination doc (\_ -> log "This won't be logged") combination2

  removeEventListener keydown listeners.keydown false doc
  removeEventListener keyup listeners.keyup false doc

  pure unit
