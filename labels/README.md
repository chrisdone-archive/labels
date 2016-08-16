# labels

Declare and access tuple fields with labels

This package is experimental, exploring the design space opened up by
the implemented and to-be-implemented work on extensible records in GHC.

*Note: You need GHC 8.0.1 for the `#foo` syntax, otherwise you have to
 use `$("foo")` which works on GHC 7.10.*

## Basic examples

The [haddock docs are here.](https://chrisdone.github.io/labels/)

Enable these extensions:

* In GHCi: `:set -XOverloadedLabels -XTypeOperators -XDataKinds -XFlexibleContexts`

* In a module: `{-# LANGUAGE OverloadedLabels, TypeOperators, DataKinds, FlexibleContexts #-}`

Let's use GHCi:

``` haskell
> import Labels
> :set -XOverloadedLabels -XTypeOperators -XDataKinds -XFlexibleContexts
```

<table>
<tr><td>Construct a record</td><td><pre lang="haskell">
> (#foo := "hi", #bar := 123)
(#foo := "hi",#bar := 123)
</pre></td></tr>
<tr><td>Get fields of a record</td><td><pre lang="haskell">
> get #bar (#foo := "hi", #bar := 123)
123
</pre></td></tr>
<tr><td>Set fields of a record</td><td><pre lang="haskell">
> set #bar 66 (#foo := "hi", #bar := 123)
(#foo := "hi",#bar := 66)
</pre></td></tr>
<tr><td>Modify fields of a record</td><td><pre lang="haskell">
> modify #mu (*0.1) (#bar := "hi", #mu := 123)
(#bar := "hi",#mu := 12.3)
</pre></td></tr>
<tr><td>Add fields to a record</td><td><pre lang="haskell">
> cons (#mu := [1,2,3]) (#foo := "hi", #bar := 123)
(#mu := [1,2,3],#foo := "hi",#bar := 123)
</pre></td></tr>
<tr><td>Abstraction</td><td><pre lang="haskell">
> let double field record = set field (get field record * 2) record
> double #mu (#bar := "hi", #mu := 123)
(#bar := "hi",#mu := 246)
</pre></td></tr>
</table>
