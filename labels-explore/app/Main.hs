{-# LANGUAGE TypeApplications, OverloadedStrings, OverloadedLabels,
  TypeOperators, DataKinds, FlexibleContexts #-}

import Labels.Explore

main1 :: IO ()
main1 =
  explore $
  httpSource "http://chrisdone.com/ontime.csv.zip" .|
  zipEntryConduit "ontime.csv" .|
  fromCsvConduit
    @("fl_date" := Day, "tail_num" := String)
    (set #downcase True csv) .|
  dropConduit 10 .|
  takeConduit 5 .>
  tableSink

main_write :: IO ()
main_write =
  explore
    (httpSource "http://chrisdone.com/ontime.csv.zip" .|
     zipEntryConduit "ontime.csv" .>
     fileSink "ontime.csv")

main1_1 :: IO ()
main1_1 =
  explore $
  fileSource "ontime.csv" .|
  fromCsvConduit
    @("fl_date" := Day, "tail_num" := String)
    (set #downcase True csv) .|
  dropConduit 10 .|
  takeConduit 5 .>
  tableSink

main2 :: IO ()
main2 =
  explore $
  fileSource "demo.csv" .|
  fromCsvConduit
    @("fl_date" := Day, "tail_num" := String, "airline_id" := Int, "unique_carrier" := String)
    (set #downcase True csv) .|
  groupConduit #airline_id .|
  explodeConduit .|
  projectConduit @("fl_date" := _, "unique_carrier" := _) .|
  takeConduit 5 .>
  tableSink

main3 :: IO ()
main3 =
  explore $
  fileSource "demo.csv" .|
  fromCsvConduit
    @("fl_date" := Day, "tail_num" := String, "airline_id" := Int, "unique_carrier" := String)
    (set #downcase True csv) .|
  groupConduit #airline_id .|
  explodeConduit .|
  dropConduit 10 .|
  projectConduit @("fl_date" := _, "tail_num" := _) .|
  takeConduit 5 .>
  tableSink

demo :: IO ()
demo = main1_1
