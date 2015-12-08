## Module Control.Monad.Eff.Keyboard

#### `preventDefault`

``` purescript
preventDefault :: forall eff. Event -> Eff (dom :: DOM | eff) Unit
```

#### `KeyboardListeners`

``` purescript
type KeyboardListeners eff = { keydown :: EventListener eff, keyup :: EventListener eff }
```

#### `KeyboardEffects`

``` purescript
type KeyboardEffects eff = (ref :: REF, err :: EXCEPTION, dom :: DOM | eff)
```

#### `onKeyCombination`

``` purescript
onKeyCombination :: forall eff. EventTarget -> (Set NormalKey -> Eff (KeyboardEffects eff) Unit) -> Set NormalKey -> Eff (KeyboardEffects eff) (KeyboardListeners (KeyboardEffects eff))
```

Calls the provided function when the specified key combination is
depressed. Returns an eff with a return value of a record which contains
the event listeners. These can be used to undo any keyboard combination
bindings created using this function.

Relies on DOM Level 3 KeyboardEvent key Values, more information is
available here http://www.w3.org/TR/DOM-Level-3-Events-key/.

If DOM Level 4 KeyboardEvent key Values aren't available in your target
browser then please bower install and add script tags for the following
polyfill.
https://github.com/inexorabletash/polyfill/blob/master/keyboard.md

It is preferable to avoid "Alt" as this changes the input characters
on some platforms.


