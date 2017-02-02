# labels

Declare and access tuple fields with labels

    $ git clone --recursive git@github.com:chrisdone/labels

## The basic package

The `labels` package just provides labels and has very small source
code and dependencies.

Some trivial examples provided by the
[labels](https://github.com/chrisdone/labels/tree/master/labels) package:

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

*Note: You need GHC 8.0.1.*

## Integration with other packages

There are integration packages for getting labelled things out of
common data parsers:

* [labels-csv](https://github.com/chrisdone/labels/tree/master/labels-csv)
* [labels-json](https://github.com/chrisdone/labels/tree/master/labels-json)

## Data exploration package

I have put together a simple packge which brings together various
packages around labels into one package:

* [labels-explore](https://github.com/chrisdone/labels/tree/master/labels-explore)
