## Module Data.Key

#### `Key`

``` purescript
newtype Key
  = Key String
```

##### Instances
``` purescript
Show Key
```

#### `NormalKey`

``` purescript
newtype NormalKey
```

##### Instances
``` purescript
Eq NormalKey
Ord NormalKey
Show NormalKey
```

#### `print`

``` purescript
print :: NormalKey -> String
```

#### `printApple`

``` purescript
printApple :: NormalKey -> String
```

#### `printCombination`

``` purescript
printCombination :: Set NormalKey -> String
```

#### `printCombinationApple`

``` purescript
printCombinationApple :: Set NormalKey -> String
```

#### `normalize`

``` purescript
normalize :: Key -> NormalKey
```

#### `normalizeCombination`

``` purescript
normalizeCombination :: Array Key -> Set NormalKey
```

#### `toApple`

``` purescript
toApple :: NormalKey -> NormalKey
```

#### `combinationToApple`

``` purescript
combinationToApple :: Set NormalKey -> Set NormalKey
```


