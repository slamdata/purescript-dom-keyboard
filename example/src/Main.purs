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
import Control.Monad.Eff.Shortcut.Platform (shortcutPlatform)
import Control.Monad.Eff.Shortcut (onShortcut)
import DOM (DOM())
import DOM.Event.EventTarget (removeEventListener)
import DOM.Event.EventTypes (keydown)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Types (documentToEventTarget)
import Data.Shortcut (Shortcut(), print, modP, rightCurlyBrace, altShiftModBackspace)
import Util (log, onLoad)

helloShortcut :: Shortcut
helloShortcut = modP

howAreYouShortcut :: Shortcut
howAreYouShortcut = rightCurlyBrace

goodbyeShortcut :: Shortcut
goodbyeShortcut = altShiftModBackspace

main :: Eff (dom :: DOM, err :: EXCEPTION) Unit
main = onLoad \_ -> do
  -- Find out the platform your program is running on.
  platform <- shortcutPlatform

  -- Retreive an `EventTarget` using purescript-dom.
  target <- map (htmlDocumentToDocument >>> documentToEventTarget) (window >>= document)

  -- Bind your actions to key shortcuts, making references to the returned
  -- event listeners if you wish to unbind later.
  onShortcut platform target (log "Hello!") helloShortcut
  onShortcut platform target (log "Goodbye!") goodbyeShortcut
  listener <- onShortcut platform target (log "How are you?") howAreYouShortcut

  -- Unbind event listeners when you are done with them.
  removeEventListener keydown listener false target

  -- Use strings to present your shortcuts as appropriate for the given platform.
  let helloShortcutString = print platform helloShortcut
  let howAreYouShortcutString = print platform howAreYouShortcut
  let goodbyeShortcutString = print platform goodbyeShortcut

  log $ helloShortcutString ++ ": appends \"Hello!\"."
  log $ howAreYouShortcutString ++ ": would append \"How are you?\" but it has been unbound."
  log $ goodbyeShortcutString ++ ": appends \"Goodbye!\"."

