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

module Data.Shortcut.Key where

import Prelude
import Data.Char (fromCharCode)
import Data.Foldable (foldr)
import Data.Maybe (fromMaybe)
import Data.Shortcut.Platform (Platform(..))
import Data.String (Pattern(Pattern), stripPrefix)
import Data.String.Capitalize (capitalize)

stripPrefix' :: String -> String -> String
stripPrefix' prefix s = fromMaybe s $ stripPrefix (Pattern prefix) s

stripPrefixes :: String -> String
stripPrefixes string = foldr stripPrefix' string ["Key", "Arrow", "Digit"]

printKeyOther :: String -> String
printKeyOther = capitalize <<< stripPrefixes

printKeyApple :: String -> String
printKeyApple "Tab" = show $ fromCharCode 8677
printKeyApple "End" = show $ fromCharCode 8600
printKeyApple "Home" = show $ fromCharCode 8598
printKeyApple "PageDown" = show $ fromCharCode 10504
printKeyApple "PageUp" = show $ fromCharCode 10505
printKeyApple "ArrowDown" = show $ fromCharCode 8595
printKeyApple "ArrowLeft" = show $ fromCharCode 8592
printKeyApple "ArrowRight" = show $ fromCharCode 8594
printKeyApple "ArrowUp" = show $ fromCharCode 8593
printKeyApple s = printKeyOther s

printKey :: Platform -> String -> String
printKey Other = printKeyOther
printKey Apple = printKeyApple

