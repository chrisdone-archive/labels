{-# LANGUAGE OverloadedStrings #-}

module Main where

import Labels.Explore

main :: IO ()
main =
  explore $ do
    httpSource "http://chrisdone.com/ontime.csv.zip" .|
      zipEntryConduit "ontime.csv" .>
      statSink

demo = main
