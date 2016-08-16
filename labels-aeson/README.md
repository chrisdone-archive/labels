# labels-aeson

Example use:

``` haskell
> import qualified Data.ByteString.Lazy as L
> json <- L.readFile "fitbit.json"
> do result :: Maybe ("activities-heart-intraday" := Value) <- fmap decode (L.readFile "fitbit.json")
     print result
Just (#activities-heart-intraday := Object (fromList [("dataset",Array [Object (fromList [("time",String "00:00:00"),("value",Number 64.0)]),Object (fromList [("time",String "00:00:10"),("value",Number 63.0)]),Object (fromList [("time",String "00:00:20"),("value",Number 64.0)]),Object (fromList [("time",String "00:00:30"),("value",Number 65.0)]),Object (fromList [("time",String "00:00:45"),("value",Number 65.0)])]),("datasetType",String "second"),("datasetInterval",Number 1.0)]))
> do result :: Maybe ("activities-heart-intraday" := ("dataset" := [("time" := String, "value" := Int)])) <- fmap decode (L.readFile "fitbit.json")
     print result
Just (#activities-heart-intraday := #dataset := [(#time := "00:00:00",#value := 64),(#time := "00:00:10",#value := 63),(#time := "00:00:20",#value := 64),(#time := "00:00:30",#value := 65),(#time := "00:00:45",#value := 65)])
> do Just (_ := (_ := ds)) :: Maybe ("activities-heart-intraday" := ("dataset" := [("time" := String, "value" := Int)])) <- fmap decode (L.readFile "fitbit.json")
     mapM_ (print) ds
     print (maximum (map (get #value) ds))
(#time := "00:00:00",#value := 64)
(#time := "00:00:10",#value := 63)
(#time := "00:00:20",#value := 64)
(#time := "00:00:30",#value := 65)
(#time := "00:00:45",#value := 65)
65
```
