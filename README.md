# labels

Declare and access tuple fields with labels

*Note: You need GHC 8.0.1 for the `#foo` syntax, otherwise you have to
 use `$("foo")`.*

## Basic examples

Enable these extensions:

* In GHCi: `:set -XOverloadedLabels -XTypeOperators -XDataKinds -XFlexibleContexts`

* In a module: `{-# LANGUAGE OverloadedLabels, TypeOperators, DataKinds, FlexibleContexts #-}`

Let's use GHCi:

``` haskell
> import Labels
> :set -XOverloadedLabels -XTypeOperators -XDataKinds -XFlexibleContexts
```

#### Construct a record

Use regular tuple syntax, but label each field with this syntax:

``` haskell
> (#foo := "hi", #bar := 123)
(#foo := "hi",#bar := 123)
```

#### Get fields of a record

Use the `get` method:

``` haskell
> get #bar (#foo := "hi", #bar := 123)
123
```

Or simply:

``` haskell
> #bar (#foo := "hi", #bar := 123)
123
```

#### Set fields of a record

Use the `set` method:

``` haskell
> set #bar 66 (#foo := "hi", #bar := 123)
(#foo := "hi",#bar := 66)
```

#### Modify fields of a record

Use `modify`, but you can easily define this in terms of `set` and `get`:

``` haskell
> modify #mu (*0.1) (#bar := "hi", #mu := 123)
(#bar := "hi",#mu := 12.3)
```

#### Add fields to a record

Use the `cons` function to add a field to a record:

``` haskell
> cons (#mu := [1,2,3]) (#foo := "hi", #bar := 123)
(#mu := [1,2,3],#foo := "hi",#bar := 123)
```

#### Abstraction

Labels are first-class and can be constructed arbitrarily and passed
around:

``` haskell
> let double field record = set field (get field record * 2) record
> double #mu (#bar := "hi", #mu := 123)
(#bar := "hi",#mu := 246)
```
