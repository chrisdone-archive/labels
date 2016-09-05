{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators, DataKinds, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Labels

--------------------------------------------------------------------------------
-- Basic
reformat
  :: (Has "text" [Char] r, Has "style" [Char] r, Has "indent" Bool r)
  => r -> [Char]
reformat kw =
  "Style: " ++
  get $("style") kw ++
  "\n" ++
  (if (get $("indent") kw)
     then "  "
     else "") ++
  (get $("text") kw)

demo :: ("foo" := (), "bar" := Char)
demo = ($("foo") := (), $("bar") := 'a')

--------------------------------------------------------------------------------
-- Consing
consed :: ("mu" := Char, "foo" := ())
consed = cons ($("mu") := 'x') ($("foo") := ())

consed' :: ("mu" := Char, "foo" := (), "bar" := Char)
consed' = cons ($("mu") := 'x') ($("foo") := (), $("bar") := 'a')

fi = cons ($("mu") := 123) ($("foo") := "hi")

bob =
  get
    $("mu")
    (set
       $("mu")
       5
       (cons ($("blah") := [1, 2, 3]) (cons ($("mu") := 123) ($("foo") := "hi"))))

--------------------------------------------------------------------------------
-- Abstraction
increment field record = set field (get field record + 1) record

foo = increment $("bar") ($("bar") := 123)