module Main where

import Debug.Trace
import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (REF())
import Control.Monad.Eff.Keyboard
import Control.Monad.Eff.Console (CONSOLE(), log)
import Data.Key
import DOM (DOM())
import DOM.HTML.Window
import DOM.HTML.Types
import DOM.HTML
import DOM.Event.EventTarget
import DOM.Event.EventTypes (keyup, keydown)
import DOM.Node.Types
import Control.Monad.Eff.Exception (EXCEPTION())
import Data.Set (Set())

combination1 :: Set NormalKey
combination1 = normalizeCombination [Key "Alt", Key "Backspace"]

combination2 :: Set NormalKey
combination2 = normalizeCombination [Key "Meta", Key "Shift", Key "A"]

main :: Eff (console :: CONSOLE, ref :: REF, dom :: DOM, err :: EXCEPTION) Unit
main = do
  -- Retreive an `EventTarget` using purescript-dom
  doc <- map (htmlDocumentToDocument >>> documentToEventTarget) (window >>= document)

  -- Bind your actions to key combinations, making a reference to the returned
  -- event listeners if you wish to unbind later.
  onKeyCombination doc (printCombinationApple >>> log) combination1
  listeners <- onKeyCombination doc (\_ -> log "This won't be logged") combination2

  -- Unbind event listeners when you are done with them.
  removeEventListener keydown listeners.keydown false doc
  removeEventListener keyup listeners.keyup false doc

  pure unit

