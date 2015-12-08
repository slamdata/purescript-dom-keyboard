# purescript-dom-keyboard

A library for capturing and manipulating key combination interactions via the DOM.

## Example
A simple example is available [here](Example.purs).

## Compatability
This package relies on DOM Level 3 KeyboardEvent key Values, more information is
available here http://www.w3.org/TR/DOM-Level-3-Events-key/.

If DOM Level 3 KeyboardEvent key Values aren't available in your target browser
then please bower install and add a script tag for the following polyfill.
https://github.com/inexorabletash/polyfill/blob/master/keyboard.md

## Documentation
Module documentation is published on Pursuit: http://pursuit.purescript.org/packages/purescript-dom-keyboard
