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

module Control.Monad.Eff.Shortcut.Platform where

import Prelude

import Control.Monad.Eff (Eff())
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Navigator (platform)
import DOM.HTML.Window (navigator)
import Data.Shortcut.Platform (Platform(), parsePlatformString)

shortcutPlatform :: forall eff. Eff (dom :: DOM | eff) Platform
shortcutPlatform = parsePlatformString <$> platformString

platformString :: forall eff. Eff (dom :: DOM | eff) String
platformString = window >>= navigator >>= platform

