{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, OverloadedLabels, TypeOperators, DataKinds, FlexibleContexts #-}

import Data.ByteString (ByteString)
import Labels.Explore

main = do
  count <-
    explore $
    fileSource "demo.csv" .|
    fromCsvConduit .|
    filterConduit
      (\(row :: ("ORIGIN" := ByteString)) -> get $("ORIGIN") row == "RKS") .>
    countSink
  print (count :: Int)
