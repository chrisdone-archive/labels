# labels

Declare and access tuple fields with labels

This package is experimental, exploring the design space opened up by
the implemented and to-be-implemented work on extensible records in GHC.

*Note: You need GHC 8.0.1 for the `#foo` syntax, otherwise you have to
 use `$("foo")` which works on GHC 7.10.*

## Basic examples

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

## Reading CSV files with Cassava

Import the instances for `FromNamedRecord`:

``` haskell
import Labels.Cassava
```

Then just specify the type you want to load:

``` haskell
> let Right (_,rows :: Vector ("salary" := Int, "name" := Text)) = decodeByName "name,salary\r\nJohn,27\r\n"
> rows
[(#salary := 27,#name := "John")]
```

Non-existent fields or invalid types result in a parse error:

``` haskell
> decodeByName "name,salary\r\nJohn,27\r\n" :: Either String (Header, Vector ("name" := Text, "age" := Int))
Left "parse error (Failed reading: conversion error: Missing field age) at \"\\r\\n\""
> decodeByName "name,salary\r\nJohn,27\r\n" :: Either String (Header, Vector ("name" := Text, "salary" := Char))
Left "parse error (Failed reading: conversion error: expected Char, got \"27\") at \"\\r\\n\""
```

Example with Yahoo!'s market data for AAPL:

``` haskell
> Right (headers,rows :: Vector ("date" := String, "high" := Double, "low" := Double)) <- fmap decodeByName (LB.readFile "AAPL.csv")
> headers
["date","open","high","low","close","volume","adj close"]
```

We can print the rows as-is:

``` haskell
> mapM_ print (V.take 2 rows)
(#date := "2016-08-10",#high := 108.900002,#low := 107.760002)
(#date := "2016-08-09",#high := 108.940002,#low := 108.010002)
```

Accessing fields is natural as anything:

``` haskell
> V.sum (V.map #low rows)
2331.789993
> let diffed = V.map (\row -> cons (#diff := (#high row - #low row)) row) rows
> mapM_ print (V.take 2 diffed)
(#diff := 1.1400000000000006,#date := "2016-08-10",#high := 108.900002,#low := 107.760002)
(#diff := 0.9300000000000068,#date := "2016-08-09",#high := 108.940002,#low := 108.010002)
```

Sometimes a CSV file will have non-valid Haskell identifiers or
spaces, e.g. `adjust close` here:

``` haskell
> Right (headers,rows :: Vector ("date" := String, "adj close" := Double)) <- fmap decodeByName (LB.readFile "AAPL.csv")
> mapM_ print (V.take 2 rows)
(#date := "2016-08-10",#adj close := 108.0)
(#date := "2016-08-09",#adj close := 108.809998)
```

Just use the `$("adj close")` syntax:

``` haskell
> mapM_ print (V.take 2 (V.map (get $("adj close")) rows))
108.0
108.809998
```

It still checks the name and type:

``` haskell
> mapM_ print (V.take 2 (V.map (get $("adj closer")) rows))
<interactive>:133:31: error:
    • No instance for (Has
                         "adj closer" a0 ("date" := String, "adj close" := Double))
        arising from a use of ‘get’
````
