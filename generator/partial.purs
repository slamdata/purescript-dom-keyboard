import Prelude hiding (zero, one)
import Data.KeyboardEvent (KeyboardEvent())
import Data.Shortcut.Modifiers (KeyModifiers(..), CodeModifiers(..), printKeyModifiers, printCodeModifiers, matchKeyModifiers, matchCodeModifiers, separator)
import Data.Shortcut.Key (printKey)
import Data.Shortcut.Platform (Platform())
import Data.String (toUpper)

data Shortcut = KeyShortcut String KeyModifiers | CodeShortcut String CodeModifiers

keyboardEventMatch :: Platform -> Shortcut -> KeyboardEvent -> Boolean
keyboardEventMatch platform (KeyShortcut key modifiers) event =
  matchKeyModifiers platform event modifiers && (toUpper key) == (toUpper event.key)
keyboardEventMatch platform (CodeShortcut code modifiers) event =
  matchCodeModifiers platform event modifiers && code == event.code

print :: Platform -> Shortcut -> String
print platform (KeyShortcut key modifiers) =
  printKeyModifiers platform modifiers ++ separator platform ++ printKey platform key
print platform (CodeShortcut code modifiers) =
  printCodeModifiers platform modifiers ++ separator platform ++ printKey platform code

