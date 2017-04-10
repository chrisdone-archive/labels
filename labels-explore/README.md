# Working with data in Haskell can be easy

In data mining or general exploration, it's common to need to easily
access data efficiently and without ceremony. Typically, a programming
language will be designed for this case specifically, like R, or a
library will be written for it, like Python with the pandas library.

Implementing this in Haskell, we improve upon this area with all the
benefits that come with using Haskell over Python or R, such as:

* Safety - garbage collected memory, no need for pointers.
* Performance - it's fast: If you write new algorithms in Haskell,
  they don't to be added to the language (like R) or written in C
  (like Python).
* Concurrency - make use of concurrent algorithms trivially, and take
  advantage of your other CPU cores.
* Maintainability - static types ensure safety at
  the time you write the program, and when you come back later to
  change them, it's harder to break them.

Let's look at an example of doing this in Haskell, and compare with
how this is done in Python's pandas. The steps are:

1. Download a zip file containing a CSV file.
1. Unzip the file.
1. Read through the CSV file.
1. Do some manipulation of the data from the file.

In Haskell we have all the libraries needed (streaming HTTP, CSV
parsing, etc.) to achieve this goal, so specifically for this post
I've made a wrapper package that brings them together like pandas
does. We have some goals:

* **Convenience** We don't want to have to write more code than
  necessary while exploring data.
* **Constant memory** Be able to process the file in constant memory
  space. If I have a 1GB file I don't want to have to load all 1GB
  into memory in one go.
* **Type-safe** I would like that once parsed from CSV, I have a
  statically-typed data structure with proper types (integers, dates,
  text, etc.).

## Python example

This example code was taken from
[Modern Pandas](http://tomaugspurger.github.io/modern-1.html). In
Python we request the web URL in chunks, which we then write to a
file. Next, we unzip the file, and then the data is available as `df`,
with column names downcased.

``` python
import zipfile
import requests
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

r = requests.get('http://chrisdone.com/misc/ontime.csv.zip', stream=True)
with open("flights.csv", 'wb') as f:
    for chunk in r.iter_content(chunk_size=1024):
        if chunk:
            f.write(chunk)

zf = zipfile.ZipFile("flights.csv.zip")
filename = zf.filelist[0].filename
fp = zf.extract(filename)
df = pd.read_csv(fp, parse_dates="FL_DATE").rename(columns=str.lower)
```

Finally, we can look at the 5 rows starting at row 10, for the columns
`fl_date` and `tail_num`, like this:

``` python
df.ix[10:14, ['fl_date', 'tail_num']]
```

=>

```
    fl_date     tail_num
10  2014-01-01  N002AA
11  2014-01-01  N3FXAA
12  2014-01-01  N906EV
13  2014-01-01  N903EV
14  2014-01-01  N903EV
```

## Python: good and bad

Good parts of the Python code:

* Extracting from the Zip file was fairly easy.
* We were able to specify for some fields to parse them as a different
  data type (`parse_dates`).
* Referencing a range of the data was easy.

Bad parts of the Python code:

* Reading the HTTP request was very verbose. We manually streamed the
  chunks to disk, which seems pointless.
* It's not statically typed. Even though we parsed `fl_date` and
  `tail_num`, we can't be certain down the line if they still exist,
  or are of the right type.
* We loaded the whole 100MB CSV into memory.

Let's compare with the solution I prepared in Haskell.

## Haskell example

I prepared the module `Labels.RunResourceT` which provides us with some
data manipulation functionality: web requests, unzipping, CSV parsing,
etc.

``` haskell
{-# LANGUAGE TypeApplications, OverloadedStrings, OverloadedLabels,
    TypeOperators, DataKinds, FlexibleContexts #-}

import Labels.RunResourceT

main =
  runResourceT $
  httpSource "http://chrisdone.com/misc/ontime.csv.zip" responseBody .|
  zipEntryConduit "ontime.csv" .|
  fromCsvConduit
    @("fl_date" := Day, "tail_num" := String)
    (set #downcase True csv) .|
  dropConduit 10 .|
  takeConduit 5 .>
  tableSink
```

Output:

``` haskell
fl_date     tail_num
2014-01-01  N002AA
2014-01-01  N3FXAA
2014-01-01  N906EV
2014-01-01  N903EV
2014-01-01  N903EV
```

Breaking this down, the `src .| c .| c .> sink` can be read like a UNIX
pipe `src | c | c > sink`.

The steps are:

* Make a web request for the zip file and yield a stream of bytes.
* Unzip that stream of bytes and yield a stream of bytes of the CSV.
* Parse from CSV into records of type `("fl_date" := Day, "tail_num" := String)`.
* Specify the `downcase` option so we can deal with lower-case names.
* Drop the first 10 results.
* Take 5 of the remaining results.
* Print the table out.

In this library the naming convention for parts of the pipline is:

* *foo*Source -- something which is at the beginning of the pipeline, a
  source of streaming input.
* *foo*Conduit -- something which connects two parts of the pipeline
  together and perhaps does some transformations (such as parsing the
  Zip, CSV or other things).
* *foo*Sink -- something into which all streaming input is poured, and
  produces a final result.

## Haskell: good parts

What's good about the Haskell version:

* Reading the HTTP request was trivial.
* It's statically typed. If I try to multiply the `fl_date` as a
  number, for example, or mistakenly write `fl_daet`, I'll get a
  compile error before ever running the program.
* It achieves it all in a streaming fashion, with constant memory
  usage.

How is it statically typed? Here:

``` haskell
fromCsvConduit
    @("fl_date" := Day, "tail_num" := String)
    csv
```

We've statically told `fromCsvConduit` the exact type of record to
construct: a record of two fields `fl_date` and `tail_num` with types
`Day` and `String`. Below, we'll look at accessing those fields in an
algorithm and demonstrate the safety aspect of this.

## Swapping out pipeline parts

We can also easily switch to reading from file. Let's write that URL
to disk, uncompressed:

``` haskell
main =
  runResourceT
    (httpSource "http://chrisdone.com/ontime.csv.zip" responseBody .|
     zipEntryConduit "ontime.csv" .>
     fileSink "ontime.csv")
```

Now our reading becomes:

``` haskell
main =
  runResourceT $
  fileSource "ontime.csv" .|
  fromCsvConduit
    @("fl_date" := Day, "tail_num" := String)
    (set #downcase True csv) .|
  dropConduit 10 .|
  takeConduit 5 .>
  tableSink
```

## Data crunching

It's easy to perform more detailed calculations. For example, to
display the number of total flights, and the total distance that would
be travelled, we can write:

``` haskell
main =
  runResourceT $
  fileSource "ontime.csv" .|
  fromCsvConduit @("distance" := Double) (set #downcase True csv) .|
  sinkConduit
    (foldSink
       (\table row ->
          modify #flights (+ 1) (modify #distance (+ get #distance row) table))
       (#flights := (0 :: Int), #distance := 0)) .>
  tableSink
```

The output is:

``` haskell
flights  distance
471949   372072490.0
```

Above we made our own sink which consumes all the rows, and then
yielded the result of that downstream to the table sink, so that we
get the nice table display at the end.

## Type correctness

Returning to our safety point, imagine above we made some mistakes.

First mistake, I wrote `modify #flights` twice by accident:

``` diff
-          modify #flights (+ 1) (modify #distance (+ get #distance row) table))
+          modify #flights (+ 1) (modify #flights (+ get #distance row) table))
```

Before running the program, the following message would be raised by
the Haskell type checker:

```
• Couldn't match type ‘Int’ with ‘Double’
  arising from a functional dependency between:
  constraint ‘Has "flights" Double ("flights" := Int, "distance" := value0)’
  arising from a use of ‘modify’
```

See below for where this information comes from in the code:

``` haskell
main =
  runResourceT $
  fileSource "ontime.csv" .|
  --
  --              The distance field is actually a double
  --                             ↓
  --
  fromCsvConduit @("distance" := Double) (set #downcase True csv) .|
  sinkConduit
    (foldSink
       (\table row ->
          modify #flights (+ 1) (modify #flights (+ get #distance row) table))
  --
  -- But we're trying to modify `#flights`, which is an `Int`.
  --                      ↓
  --
       (#flights := (0 :: Int), #distance := 0)) .>
  tableSink
```

Likewise, if we misspelled `#distance` as `#distant`, in our algorithm:

``` diff
-          modify #flights (+ 1) (modify #distance (+ get #distance row) table))
+          modify #flights (+ 1) (modify #distance (+ get #distant row) table))
```

We would get this error message:

```
No instance for (Has "distant" value0 ("distance" := Double))
arising from a use of ‘get’
```

Summarizing:

* The correct values being parsed from the CSV.
* Fields must exist if we're accessing them.
* We can't mismatch types.

All this adds up to more maintainable software, and yet we didn't have
to state any more than necessary!

## Grouping

If instead we'd like to group by a field, in pandas it's like this:

``` python
first = df.groupby('airline_id')[['fl_date', 'unique_carrier']].first()
first.head()
```

We simply update the code with the type, putting the additional fields
we want to parse:

``` haskell
csv :: Csv ("fl_date" := Day, "tail_num" := String
           ,"airline_id" := Int, "unique_carrier" := String)
```

And then our pipeline instead becomes:

``` haskell
fromCsvConduit
  @("fl_date" := Day, "tail_num" := String,
    "airline_id" := Int, "unique_carrier" := String)
  (set #downcase True csv) .|
groupConduit #airline_id .|
explodeConduit .|
projectConduit @("fl_date" := _, "unique_carrier" := _) .|
takeConduit 5 .>
tableSink
```

* We added the two new fields to be parsed.
* We grouped by the `#airline_id` field into a stream of lists of
  rows. That groups the stream `[x,y,z,a,b,c]` into
  e.g. `[[x,y],[z,a],[b,c]]`.
* We explode those groups `[[x,y],[z,a],[b,c],...]` into a stream of
  each group's parts: `[x,y,z,a,b,c,...]`.
* We _projected_ a new record type just for the table display to
  include `fl_date` and `unique_carrier`. The types are to be left
  as-is, so we use `_` to mean "you know what I mean". This is like
  `SELECT fl_date, unique_carrier` in SQL.

Output:


``` haskell
unique_carrier  fl_date
AA              2014-01-01
AA              2014-01-01
EV              2014-01-01
EV              2014-01-01
EV              2014-01-01
```

The Python blog post states that a further query upon that result,

``` python
first.ix[10:15, ['fl_date', 'tail_num']]
```

yields an unexpected empty data frame, due to strange indexing
behaviour of pandas. But ours works out fine, we just drop 10 elements
from the input stream and project `tail_num` instead:

``` haskell
dropConduit 10 .|
projectConduit @("fl_date" := _, "tail_num" := _) .|
takeConduit 5 .>
tableSink
```

And we get

``` haskell
fl_date     tail_num
2014-01-01  N002AA
2014-01-01  N3FXAA
2014-01-01  N906EV
2014-01-01  N903EV
2014-01-01  N903EV
```

## Conclusion

In this post we've demonstrated:

1. Concisely handling a chain of problems smoothly like a bash
   script.
2. Done all the above in constant memory usage.
3. Done so with a type-safe parser, specifying our types statically,
   but without having to declare or name any record type ahead of
   time.

Are the advantages of using Haskell something you're interested in? If
so, [contact us.](https://www.fpcomplete.com/contact-us)
