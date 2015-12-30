# purescript-dom-keyboard

A library for binding actions to and presenting DOM keyboard shortcuts.

## Shortcuts

The shortcuts available are strongly typed to help ensure that your shortcuts
are triggered and prevent applications being hidden or closed across various
platforms and locales.

If you find an available shortcut hides or closes an application or that a
shortcut isn't triggered on some platform or locale please open an issue. Pull
requests are very welcome.

## Installation

```
bower install purescript-dom-keyboard
```

## Compatability

This library relies on
[DOM Level 3 `KeyboardEvent.key`](http://www.w3.org/TR/DOM-Level-3-Events-key/)
and [DOM Level 3 `KeyboardEvent.code`](http://www.w3.org/TR/DOM-Level-3-Events-code/)
values. If your target browser does not support these then please consider using
a polyfill such as the one provided by
[`js-polyfills`](https://github.com/inexorabletash/polyfill/blob/master/keyboard.md).

## Example

A simple example is available [here](example/src/Main.purs).

It can be run by using `pulp server` from the `example` directory and navigating
to `http://localhost:1337` in a web browser.

## Documentation

Module documentation is published on Pursuit:
http://pursuit.purescript.org/packages/purescript-dom-keyboard

## Development

To add or remove keyboard shortcuts please use
[the provided program generator](generator/src/Main.purs).

After altering the key or code shortcut sources use `pulp run --include ../src`
in the `generator` directory to update `src/Data/Shortcut.purs`.

