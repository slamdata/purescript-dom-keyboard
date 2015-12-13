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

module Util (log, onLoad) where

import Prelude

import Control.Bind ((<=<), (=<<))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (MonadEff, liftEff)
import DOM (DOM())
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.Event.EventTypes (load)
import DOM.Event.Types (Event())
import DOM.HTML (window)
import DOM.HTML.Types (Window(), windowToEventTarget, htmlDocumentToParentNode, htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.Document (createElement, createTextNode)
import DOM.Node.Node (appendChild)
import DOM.Node.ParentNode (querySelector)
import DOM.Node.Types (Node(), Element())
import DOM.Node.Types (textToNode, elementToNode)
import Data.Maybe (maybe)
import Data.Nullable (Nullable(), toMaybe)

appendToBody :: forall eff. Node -> Eff (dom :: DOM | eff) Unit
appendToBody node = maybe (pure unit) (appendNodeToElement node) =<< toMaybe <$> (body =<< window)

appendNodeToElement :: forall eff. Node -> Element -> Eff ( dom :: DOM | eff) Unit
appendNodeToElement node = void <<< appendChild node <<< elementToNode

body :: forall eff. Window -> Eff ( dom :: DOM | eff) (Nullable Element)
body = querySelector "body" <<< htmlDocumentToParentNode <=< document

onLoad :: forall eff. (Event -> Eff (dom :: DOM | eff) Unit) -> Eff (dom :: DOM | eff) Unit
onLoad callback = liftEff $ addEventListener load (eventListener callback) false <<< windowToEventTarget =<< window

log :: forall eff. String -> Eff (dom :: DOM | eff) Unit
log s = do
  doc <- htmlDocumentToDocument <$> (window >>= document)
  textNode <- createTextNode s doc >>= textToNode >>> pure
  pNode <- createElement "p" doc >>= elementToNode >>> pure
  appendToBody pNode
  appendChild textNode pNode
  pure unit

