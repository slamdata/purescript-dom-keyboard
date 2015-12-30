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

module Data.Shortcut
  ( Shortcut()
  , print
  , keyboardEventMatch
  , a
  , modA
  , b
  , modB
  , c
  , modC
  , d
  , modD
  , e
  , modE
  , f
  , modF
  , g
  , modG
  , h
  , modH
  , i
  , modI
  , j
  , modJ
  , k
  , modK
  , l
  , modL
  , m
  , n
  , modN
  , o
  , modO
  , p
  , modP
  , q
  , r
  , modR
  , s
  , modS
  , t
  , modT
  , u
  , modU
  , v
  , modV
  , w
  , x
  , modX
  , y
  , modY
  , z
  , modZ
  , semicolon
  , modSemicolon
  , colon
  , modColon
  , backtick
  , modBacktick
  , plus
  , modPlus
  , minus
  , modMinus
  , equals
  , modEquals
  , underscore
  , modUnderscore
  , leftBracket
  , modLeftBracket
  , rightBracket
  , modRightBracket
  , leftCurlyBrace
  , modLeftCurlyBrace
  , rightCurlyBrace
  , modRightCurlyBrace
  , forwardSlash
  , modForwardSlash
  , backSlash
  , modBackSlash
  , period
  , modPeriod
  , comma
  , modComma
  , zero
  , altZero
  , shiftZero
  , modZero
  , altShiftZero
  , altModZero
  , shiftModZero
  , altShiftModZero
  , one
  , altOne
  , shiftOne
  , altShiftOne
  , altModOne
  , shiftModOne
  , altShiftModOne
  , two
  , altTwo
  , shiftTwo
  , altShiftTwo
  , altModTwo
  , shiftModTwo
  , altShiftModTwo
  , three
  , altThree
  , shiftThree
  , altShiftThree
  , altModThree
  , altShiftModThree
  , four
  , altFour
  , shiftFour
  , altShiftFour
  , altModFour
  , altShiftModFour
  , five
  , altFive
  , shiftFive
  , altShiftFive
  , altModFive
  , shiftModFive
  , altShiftModFive
  , six
  , altSix
  , shiftSix
  , altShiftSix
  , altModSix
  , shiftModSix
  , altShiftModSix
  , seven
  , altSeven
  , shiftSeven
  , altShiftSeven
  , altModSeven
  , shiftModSeven
  , altShiftModSeven
  , eight
  , altEight
  , shiftEight
  , altShiftEight
  , altModEight
  , shiftModEight
  , altShiftModEight
  , nine
  , altNine
  , shiftNine
  , altShiftNine
  , altModNine
  , shiftModNine
  , altShiftModNine
  , space
  , altSpace
  , shiftSpace
  , altShiftSpace
  , shiftModSpace
  , altShiftModSpace
  , tab
  , shiftTab
  , altModTab
  , altShiftModTab
  , enter
  , altEnter
  , shiftEnter
  , modEnter
  , altShiftEnter
  , altModEnter
  , shiftModEnter
  , altShiftModEnter
  , home
  , altHome
  , shiftHome
  , modHome
  , altShiftHome
  , altModHome
  , shiftModHome
  , altShiftModHome
  , end
  , altEnd
  , shiftEnd
  , modEnd
  , altShiftEnd
  , altModEnd
  , shiftModEnd
  , altShiftModEnd
  , pageDown
  , altPageDown
  , shiftPageDown
  , modPageDown
  , altShiftPageDown
  , altModPageDown
  , shiftModPageDown
  , altShiftModPageDown
  , pageUp
  , altPageUp
  , shiftPageUp
  , modPageUp
  , altShiftPageUp
  , altModPageUp
  , shiftModPageUp
  , altShiftModPageUp
  , up
  , altUp
  , shiftUp
  , modUp
  , altShiftUp
  , altModUp
  , shiftModUp
  , altShiftModUp
  , down
  , altDown
  , shiftDown
  , modDown
  , altShiftDown
  , altModDown
  , shiftModDown
  , altShiftModDown
  , left
  , altLeft
  , shiftLeft
  , modLeft
  , altShiftLeft
  , altModLeft
  , shiftModLeft
  , altShiftModLeft
  , right
  , altRight
  , shiftRight
  , modRight
  , altShiftRight
  , altModRight
  , shiftModRight
  , altShiftModRight
  , backspace
  , altBackspace
  , shiftBackspace
  , modBackspace
  , altShiftBackspace
  , altModBackspace
  , shiftModBackspace
  , altShiftModBackspace
  ) where

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

a :: Shortcut
a = KeyShortcut "a" KeyNone

modA :: Shortcut
modA = KeyShortcut "a" KeyMod

b :: Shortcut
b = KeyShortcut "b" KeyNone

modB :: Shortcut
modB = KeyShortcut "b" KeyMod

c :: Shortcut
c = KeyShortcut "c" KeyNone

modC :: Shortcut
modC = KeyShortcut "c" KeyMod

d :: Shortcut
d = KeyShortcut "d" KeyNone

modD :: Shortcut
modD = KeyShortcut "d" KeyMod

e :: Shortcut
e = KeyShortcut "e" KeyNone

modE :: Shortcut
modE = KeyShortcut "e" KeyMod

f :: Shortcut
f = KeyShortcut "f" KeyNone

modF :: Shortcut
modF = KeyShortcut "f" KeyMod

g :: Shortcut
g = KeyShortcut "g" KeyNone

modG :: Shortcut
modG = KeyShortcut "g" KeyMod

h :: Shortcut
h = KeyShortcut "h" KeyNone

modH :: Shortcut
modH = KeyShortcut "h" KeyMod

i :: Shortcut
i = KeyShortcut "i" KeyNone

modI :: Shortcut
modI = KeyShortcut "i" KeyMod

j :: Shortcut
j = KeyShortcut "j" KeyNone

modJ :: Shortcut
modJ = KeyShortcut "j" KeyMod

k :: Shortcut
k = KeyShortcut "k" KeyNone

modK :: Shortcut
modK = KeyShortcut "k" KeyMod

l :: Shortcut
l = KeyShortcut "l" KeyNone

modL :: Shortcut
modL = KeyShortcut "l" KeyMod

m :: Shortcut
m = KeyShortcut "m" KeyNone

n :: Shortcut
n = KeyShortcut "n" KeyNone

modN :: Shortcut
modN = KeyShortcut "n" KeyMod

o :: Shortcut
o = KeyShortcut "o" KeyNone

modO :: Shortcut
modO = KeyShortcut "o" KeyMod

p :: Shortcut
p = KeyShortcut "p" KeyNone

modP :: Shortcut
modP = KeyShortcut "p" KeyMod

q :: Shortcut
q = KeyShortcut "q" KeyNone

r :: Shortcut
r = KeyShortcut "r" KeyNone

modR :: Shortcut
modR = KeyShortcut "r" KeyMod

s :: Shortcut
s = KeyShortcut "s" KeyNone

modS :: Shortcut
modS = KeyShortcut "s" KeyMod

t :: Shortcut
t = KeyShortcut "t" KeyNone

modT :: Shortcut
modT = KeyShortcut "t" KeyMod

u :: Shortcut
u = KeyShortcut "u" KeyNone

modU :: Shortcut
modU = KeyShortcut "u" KeyMod

v :: Shortcut
v = KeyShortcut "v" KeyNone

modV :: Shortcut
modV = KeyShortcut "v" KeyMod

w :: Shortcut
w = KeyShortcut "w" KeyNone

x :: Shortcut
x = KeyShortcut "x" KeyNone

modX :: Shortcut
modX = KeyShortcut "x" KeyMod

y :: Shortcut
y = KeyShortcut "y" KeyNone

modY :: Shortcut
modY = KeyShortcut "y" KeyMod

z :: Shortcut
z = KeyShortcut "z" KeyNone

modZ :: Shortcut
modZ = KeyShortcut "z" KeyMod

semicolon :: Shortcut
semicolon = KeyShortcut ";" KeyNone

modSemicolon :: Shortcut
modSemicolon = KeyShortcut ";" KeyMod

colon :: Shortcut
colon = KeyShortcut ":" KeyNone

modColon :: Shortcut
modColon = KeyShortcut ":" KeyMod

backtick :: Shortcut
backtick = KeyShortcut "`" KeyNone

modBacktick :: Shortcut
modBacktick = KeyShortcut "`" KeyMod

plus :: Shortcut
plus = KeyShortcut "+" KeyNone

modPlus :: Shortcut
modPlus = KeyShortcut "+" KeyMod

minus :: Shortcut
minus = KeyShortcut "-" KeyNone

modMinus :: Shortcut
modMinus = KeyShortcut "-" KeyMod

equals :: Shortcut
equals = KeyShortcut "=" KeyNone

modEquals :: Shortcut
modEquals = KeyShortcut "=" KeyMod

underscore :: Shortcut
underscore = KeyShortcut "_" KeyNone

modUnderscore :: Shortcut
modUnderscore = KeyShortcut "_" KeyMod

leftBracket :: Shortcut
leftBracket = KeyShortcut "[" KeyNone

modLeftBracket :: Shortcut
modLeftBracket = KeyShortcut "[" KeyMod

rightBracket :: Shortcut
rightBracket = KeyShortcut "]" KeyNone

modRightBracket :: Shortcut
modRightBracket = KeyShortcut "]" KeyMod

leftCurlyBrace :: Shortcut
leftCurlyBrace = KeyShortcut "{" KeyNone

modLeftCurlyBrace :: Shortcut
modLeftCurlyBrace = KeyShortcut "{" KeyMod

rightCurlyBrace :: Shortcut
rightCurlyBrace = KeyShortcut "}" KeyNone

modRightCurlyBrace :: Shortcut
modRightCurlyBrace = KeyShortcut "}" KeyMod

forwardSlash :: Shortcut
forwardSlash = KeyShortcut "/" KeyNone

modForwardSlash :: Shortcut
modForwardSlash = KeyShortcut "/" KeyMod

backSlash :: Shortcut
backSlash = KeyShortcut "\\" KeyNone

modBackSlash :: Shortcut
modBackSlash = KeyShortcut "\\" KeyMod

period :: Shortcut
period = KeyShortcut "." KeyNone

modPeriod :: Shortcut
modPeriod = KeyShortcut "." KeyMod

comma :: Shortcut
comma = KeyShortcut "," KeyNone

modComma :: Shortcut
modComma = KeyShortcut "," KeyMod

zero :: Shortcut
zero = CodeShortcut "Digit0" None

altZero :: Shortcut
altZero = CodeShortcut "Digit0" Alt

shiftZero :: Shortcut
shiftZero = CodeShortcut "Digit0" Shift

modZero :: Shortcut
modZero = CodeShortcut "Digit0" Mod

altShiftZero :: Shortcut
altShiftZero = CodeShortcut "Digit0" AltShift

altModZero :: Shortcut
altModZero = CodeShortcut "Digit0" AltMod

shiftModZero :: Shortcut
shiftModZero = CodeShortcut "Digit0" ShiftMod

altShiftModZero :: Shortcut
altShiftModZero = CodeShortcut "Digit0" AltShiftMod

one :: Shortcut
one = CodeShortcut "Digit1" None

altOne :: Shortcut
altOne = CodeShortcut "Digit1" Alt

shiftOne :: Shortcut
shiftOne = CodeShortcut "Digit1" Shift

altShiftOne :: Shortcut
altShiftOne = CodeShortcut "Digit1" AltShift

altModOne :: Shortcut
altModOne = CodeShortcut "Digit1" AltMod

shiftModOne :: Shortcut
shiftModOne = CodeShortcut "Digit1" ShiftMod

altShiftModOne :: Shortcut
altShiftModOne = CodeShortcut "Digit1" AltShiftMod

two :: Shortcut
two = CodeShortcut "Digit2" None

altTwo :: Shortcut
altTwo = CodeShortcut "Digit2" Alt

shiftTwo :: Shortcut
shiftTwo = CodeShortcut "Digit2" Shift

altShiftTwo :: Shortcut
altShiftTwo = CodeShortcut "Digit2" AltShift

altModTwo :: Shortcut
altModTwo = CodeShortcut "Digit2" AltMod

shiftModTwo :: Shortcut
shiftModTwo = CodeShortcut "Digit2" ShiftMod

altShiftModTwo :: Shortcut
altShiftModTwo = CodeShortcut "Digit2" AltShiftMod

three :: Shortcut
three = CodeShortcut "Digit3" None

altThree :: Shortcut
altThree = CodeShortcut "Digit3" Alt

shiftThree :: Shortcut
shiftThree = CodeShortcut "Digit3" Shift

altShiftThree :: Shortcut
altShiftThree = CodeShortcut "Digit3" AltShift

altModThree :: Shortcut
altModThree = CodeShortcut "Digit3" AltMod

altShiftModThree :: Shortcut
altShiftModThree = CodeShortcut "Digit3" AltShiftMod

four :: Shortcut
four = CodeShortcut "Digit4" None

altFour :: Shortcut
altFour = CodeShortcut "Digit4" Alt

shiftFour :: Shortcut
shiftFour = CodeShortcut "Digit4" Shift

altShiftFour :: Shortcut
altShiftFour = CodeShortcut "Digit4" AltShift

altModFour :: Shortcut
altModFour = CodeShortcut "Digit4" AltMod

altShiftModFour :: Shortcut
altShiftModFour = CodeShortcut "Digit4" AltShiftMod

five :: Shortcut
five = CodeShortcut "Digit5" None

altFive :: Shortcut
altFive = CodeShortcut "Digit5" Alt

shiftFive :: Shortcut
shiftFive = CodeShortcut "Digit5" Shift

altShiftFive :: Shortcut
altShiftFive = CodeShortcut "Digit5" AltShift

altModFive :: Shortcut
altModFive = CodeShortcut "Digit5" AltMod

shiftModFive :: Shortcut
shiftModFive = CodeShortcut "Digit5" ShiftMod

altShiftModFive :: Shortcut
altShiftModFive = CodeShortcut "Digit5" AltShiftMod

six :: Shortcut
six = CodeShortcut "Digit6" None

altSix :: Shortcut
altSix = CodeShortcut "Digit6" Alt

shiftSix :: Shortcut
shiftSix = CodeShortcut "Digit6" Shift

altShiftSix :: Shortcut
altShiftSix = CodeShortcut "Digit6" AltShift

altModSix :: Shortcut
altModSix = CodeShortcut "Digit6" AltMod

shiftModSix :: Shortcut
shiftModSix = CodeShortcut "Digit6" ShiftMod

altShiftModSix :: Shortcut
altShiftModSix = CodeShortcut "Digit6" AltShiftMod

seven :: Shortcut
seven = CodeShortcut "Digit7" None

altSeven :: Shortcut
altSeven = CodeShortcut "Digit7" Alt

shiftSeven :: Shortcut
shiftSeven = CodeShortcut "Digit7" Shift

altShiftSeven :: Shortcut
altShiftSeven = CodeShortcut "Digit7" AltShift

altModSeven :: Shortcut
altModSeven = CodeShortcut "Digit7" AltMod

shiftModSeven :: Shortcut
shiftModSeven = CodeShortcut "Digit7" ShiftMod

altShiftModSeven :: Shortcut
altShiftModSeven = CodeShortcut "Digit7" AltShiftMod

eight :: Shortcut
eight = CodeShortcut "Digit8" None

altEight :: Shortcut
altEight = CodeShortcut "Digit8" Alt

shiftEight :: Shortcut
shiftEight = CodeShortcut "Digit8" Shift

altShiftEight :: Shortcut
altShiftEight = CodeShortcut "Digit8" AltShift

altModEight :: Shortcut
altModEight = CodeShortcut "Digit8" AltMod

shiftModEight :: Shortcut
shiftModEight = CodeShortcut "Digit8" ShiftMod

altShiftModEight :: Shortcut
altShiftModEight = CodeShortcut "Digit8" AltShiftMod

nine :: Shortcut
nine = CodeShortcut "Digit9" None

altNine :: Shortcut
altNine = CodeShortcut "Digit9" Alt

shiftNine :: Shortcut
shiftNine = CodeShortcut "Digit9" Shift

altShiftNine :: Shortcut
altShiftNine = CodeShortcut "Digit9" AltShift

altModNine :: Shortcut
altModNine = CodeShortcut "Digit9" AltMod

shiftModNine :: Shortcut
shiftModNine = CodeShortcut "Digit9" ShiftMod

altShiftModNine :: Shortcut
altShiftModNine = CodeShortcut "Digit9" AltShiftMod

space :: Shortcut
space = CodeShortcut "Space" None

altSpace :: Shortcut
altSpace = CodeShortcut "Space" Alt

shiftSpace :: Shortcut
shiftSpace = CodeShortcut "Space" Shift

altShiftSpace :: Shortcut
altShiftSpace = CodeShortcut "Space" AltShift

shiftModSpace :: Shortcut
shiftModSpace = CodeShortcut "Space" ShiftMod

altShiftModSpace :: Shortcut
altShiftModSpace = CodeShortcut "Space" AltShiftMod

tab :: Shortcut
tab = CodeShortcut "Tab" None

shiftTab :: Shortcut
shiftTab = CodeShortcut "Tab" Shift

altModTab :: Shortcut
altModTab = CodeShortcut "Tab" AltMod

altShiftModTab :: Shortcut
altShiftModTab = CodeShortcut "Tab" AltShiftMod

enter :: Shortcut
enter = CodeShortcut "Enter" None

altEnter :: Shortcut
altEnter = CodeShortcut "Enter" Alt

shiftEnter :: Shortcut
shiftEnter = CodeShortcut "Enter" Shift

modEnter :: Shortcut
modEnter = CodeShortcut "Enter" Mod

altShiftEnter :: Shortcut
altShiftEnter = CodeShortcut "Enter" AltShift

altModEnter :: Shortcut
altModEnter = CodeShortcut "Enter" AltMod

shiftModEnter :: Shortcut
shiftModEnter = CodeShortcut "Enter" ShiftMod

altShiftModEnter :: Shortcut
altShiftModEnter = CodeShortcut "Enter" AltShiftMod

home :: Shortcut
home = CodeShortcut "Home" None

altHome :: Shortcut
altHome = CodeShortcut "Home" Alt

shiftHome :: Shortcut
shiftHome = CodeShortcut "Home" Shift

modHome :: Shortcut
modHome = CodeShortcut "Home" Mod

altShiftHome :: Shortcut
altShiftHome = CodeShortcut "Home" AltShift

altModHome :: Shortcut
altModHome = CodeShortcut "Home" AltMod

shiftModHome :: Shortcut
shiftModHome = CodeShortcut "Home" ShiftMod

altShiftModHome :: Shortcut
altShiftModHome = CodeShortcut "Home" AltShiftMod

end :: Shortcut
end = CodeShortcut "End" None

altEnd :: Shortcut
altEnd = CodeShortcut "End" Alt

shiftEnd :: Shortcut
shiftEnd = CodeShortcut "End" Shift

modEnd :: Shortcut
modEnd = CodeShortcut "End" Mod

altShiftEnd :: Shortcut
altShiftEnd = CodeShortcut "End" AltShift

altModEnd :: Shortcut
altModEnd = CodeShortcut "End" AltMod

shiftModEnd :: Shortcut
shiftModEnd = CodeShortcut "End" ShiftMod

altShiftModEnd :: Shortcut
altShiftModEnd = CodeShortcut "End" AltShiftMod

pageDown :: Shortcut
pageDown = CodeShortcut "PageDown" None

altPageDown :: Shortcut
altPageDown = CodeShortcut "PageDown" Alt

shiftPageDown :: Shortcut
shiftPageDown = CodeShortcut "PageDown" Shift

modPageDown :: Shortcut
modPageDown = CodeShortcut "PageDown" Mod

altShiftPageDown :: Shortcut
altShiftPageDown = CodeShortcut "PageDown" AltShift

altModPageDown :: Shortcut
altModPageDown = CodeShortcut "PageDown" AltMod

shiftModPageDown :: Shortcut
shiftModPageDown = CodeShortcut "PageDown" ShiftMod

altShiftModPageDown :: Shortcut
altShiftModPageDown = CodeShortcut "PageDown" AltShiftMod

pageUp :: Shortcut
pageUp = CodeShortcut "PageUp" None

altPageUp :: Shortcut
altPageUp = CodeShortcut "PageUp" Alt

shiftPageUp :: Shortcut
shiftPageUp = CodeShortcut "PageUp" Shift

modPageUp :: Shortcut
modPageUp = CodeShortcut "PageUp" Mod

altShiftPageUp :: Shortcut
altShiftPageUp = CodeShortcut "PageUp" AltShift

altModPageUp :: Shortcut
altModPageUp = CodeShortcut "PageUp" AltMod

shiftModPageUp :: Shortcut
shiftModPageUp = CodeShortcut "PageUp" ShiftMod

altShiftModPageUp :: Shortcut
altShiftModPageUp = CodeShortcut "PageUp" AltShiftMod

up :: Shortcut
up = CodeShortcut "ArrowUp" None

altUp :: Shortcut
altUp = CodeShortcut "ArrowUp" Alt

shiftUp :: Shortcut
shiftUp = CodeShortcut "ArrowUp" Shift

modUp :: Shortcut
modUp = CodeShortcut "ArrowUp" Mod

altShiftUp :: Shortcut
altShiftUp = CodeShortcut "ArrowUp" AltShift

altModUp :: Shortcut
altModUp = CodeShortcut "ArrowUp" AltMod

shiftModUp :: Shortcut
shiftModUp = CodeShortcut "ArrowUp" ShiftMod

altShiftModUp :: Shortcut
altShiftModUp = CodeShortcut "ArrowUp" AltShiftMod

down :: Shortcut
down = CodeShortcut "ArrowDown" None

altDown :: Shortcut
altDown = CodeShortcut "ArrowDown" Alt

shiftDown :: Shortcut
shiftDown = CodeShortcut "ArrowDown" Shift

modDown :: Shortcut
modDown = CodeShortcut "ArrowDown" Mod

altShiftDown :: Shortcut
altShiftDown = CodeShortcut "ArrowDown" AltShift

altModDown :: Shortcut
altModDown = CodeShortcut "ArrowDown" AltMod

shiftModDown :: Shortcut
shiftModDown = CodeShortcut "ArrowDown" ShiftMod

altShiftModDown :: Shortcut
altShiftModDown = CodeShortcut "ArrowDown" AltShiftMod

left :: Shortcut
left = CodeShortcut "ArrowLeft" None

altLeft :: Shortcut
altLeft = CodeShortcut "ArrowLeft" Alt

shiftLeft :: Shortcut
shiftLeft = CodeShortcut "ArrowLeft" Shift

modLeft :: Shortcut
modLeft = CodeShortcut "ArrowLeft" Mod

altShiftLeft :: Shortcut
altShiftLeft = CodeShortcut "ArrowLeft" AltShift

altModLeft :: Shortcut
altModLeft = CodeShortcut "ArrowLeft" AltMod

shiftModLeft :: Shortcut
shiftModLeft = CodeShortcut "ArrowLeft" ShiftMod

altShiftModLeft :: Shortcut
altShiftModLeft = CodeShortcut "ArrowLeft" AltShiftMod

right :: Shortcut
right = CodeShortcut "ArrowRight" None

altRight :: Shortcut
altRight = CodeShortcut "ArrowRight" Alt

shiftRight :: Shortcut
shiftRight = CodeShortcut "ArrowRight" Shift

modRight :: Shortcut
modRight = CodeShortcut "ArrowRight" Mod

altShiftRight :: Shortcut
altShiftRight = CodeShortcut "ArrowRight" AltShift

altModRight :: Shortcut
altModRight = CodeShortcut "ArrowRight" AltMod

shiftModRight :: Shortcut
shiftModRight = CodeShortcut "ArrowRight" ShiftMod

altShiftModRight :: Shortcut
altShiftModRight = CodeShortcut "ArrowRight" AltShiftMod

backspace :: Shortcut
backspace = CodeShortcut "Backspace" None

altBackspace :: Shortcut
altBackspace = CodeShortcut "Backspace" Alt

shiftBackspace :: Shortcut
shiftBackspace = CodeShortcut "Backspace" Shift

modBackspace :: Shortcut
modBackspace = CodeShortcut "Backspace" Mod

altShiftBackspace :: Shortcut
altShiftBackspace = CodeShortcut "Backspace" AltShift

altModBackspace :: Shortcut
altModBackspace = CodeShortcut "Backspace" AltMod

shiftModBackspace :: Shortcut
shiftModBackspace = CodeShortcut "Backspace" ShiftMod

altShiftModBackspace :: Shortcut
altShiftModBackspace = CodeShortcut "Backspace" AltShiftMod

