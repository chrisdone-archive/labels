# labels

Declare and access tuple fields with labels

## Basic examples

Enable these extensions:

* In GHCi: `:set -XOverloadedLabels -XTypeOperators -XDataKinds -XFlexibleContexts`

* In a module: `{-# LANGUAGE OverloadedLabels, TypeOperators, DataKinds, FlexibleContexts #-}`

Construct a record:

``` haskell
> (#foo := "hi", #bar := 123)
(#foo := "hi",#bar := 123)
```

Get fields of a record:

``` haskell
> get #bar (#foo := "hi", #bar := 123)
123
```

Set fields of a record:

``` haskell
> set #bar 66 (#foo := "hi", #bar := 123)
(#foo := "hi",#bar := 66)
```

Modify fields of a record:

``` haskell
> modify #mu (*0.1) (#bar := "hi", #mu := 123)
(#bar := "hi",#mu := 12.3)
```

Add fields to a record:

``` haskell
> cons (#mu := [1,2,3]) (#foo := "hi", #bar := 123)
(#mu := [1,2,3],#foo := "hi",#bar := 123)
```

Abstraction:

``` haskell
> let double field record = set field (get field record * 2) record
> double #mu (#bar := "hi", #mu := 123)
(#bar := "hi",#mu := 246)
```
