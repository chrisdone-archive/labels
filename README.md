# labels

Declare and access tuple fields with labels

This package is experimental, exploring the design space opened up by
the implemented and to-be-implemented work on extensible records in GHC.

*Note: You need GHC 8.0.1 for the `#foo` syntax, otherwise you have to
 use `$("foo")` which works on GHC 7.10.*

## Basic examples

Some trivial examples provided by the
[labels](https://github.com/chrisdone/labels/tree/master/labels) package.

<table>
<tr><td>Get fields of a record</td><td><pre lang="haskell">
> get #bar (#foo := "hi", #bar := 123)
123
</pre></td></tr>
<tr><td>Set fields of a record</td><td><pre lang="haskell">
> set #bar 66 (#foo := "hi", #bar := 123)
(#foo := "hi",#bar := 66)
</pre></td></tr>
</table>

## Integrations with packages

There are integration packages for getting labelled things out of
common data parsers:

* [labels-cassava](https://github.com/chrisdone/labels/tree/master/labels-cassava)
* [labels-aeson](https://github.com/chrisdone/labels/tree/master/labels-aeson)
