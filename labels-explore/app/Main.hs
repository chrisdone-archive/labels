{-# LANGUAGE TypeApplications, OverloadedStrings, OverloadedLabels,
  TypeOperators, DataKinds, FlexibleContexts #-}

import Labels.Explore

moo1 :: IO ()
moo1 =
  runResourceT $
  httpSource "http://chrisdone.com/ontime.csv.zip" responseBody .|
  zipEntryConduit "ontime.csv" .|
  fromCsvConduit
    @("fl_date" := Day, "tail_num" := String)
    (set #downcase True csv) .|
  dropConduit 10 .|
  takeConduit 5 .>
  tableSink

moo_write :: IO ()
moo_write =
  runResourceT
    (httpSource "http://chrisdone.com/ontime.csv.zip" responseBody .|
     zipEntryConduit "ontime.csv" .>
     fileSink "ontime.csv")

moo1_1 :: IO ()
moo1_1 =
  runResourceT $
  fileSource "ontime.csv" .|
  fromCsvConduit
    @("distance" := Double)
    (set #downcase True csv) .|
  sinkConduit
    (foldSink
       (\table row ->
          modify #flights (+ 1) (modify #distance (+ get #distance row) table))
       (#flights := (0 :: Int), #distance := 0)) .>
  tableSink

moo2 :: IO ()
moo2 =
  runResourceT $
  fileSource "ontime.csv" .|
  fromCsvConduit
    @("fl_date" := Day, "tail_num" := String, "airline_id" := Int, "unique_carrier" := String)
    (set #downcase True csv) .|
  groupConduit #airline_id .|
  explodeConduit .|
  projectConduit @("fl_date" := _, "unique_carrier" := _) .|
  takeConduit 5 .>
  tableSink

moo3 :: IO ()
moo3 =
  runResourceT $
  fileSource "ontime.csv" .|
  fromCsvConduit
    @("fl_date" := Day, "tail_num" := String, "airline_id" := Int, "unique_carrier" := String)
    (set #downcase True csv) .|
  groupConduit #airline_id .|
  explodeConduit .|
  dropConduit 10 .|
  projectConduit @("fl_date" := _, "tail_num" := _) .|
  takeConduit 5 .>
  tableSink

main = moo1_1
