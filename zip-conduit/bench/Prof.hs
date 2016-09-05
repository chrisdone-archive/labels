module Main where

import           System.FilePath
import           System.Environment

import           Control.DeepSeq
import           System.IO.Temp

import           Codec.Archive.Zip


main :: IO ()
main = do
    path:_ <- getArgs

    withSystemTempDirectory "zip-conduit" $ \dir -> do
        withArchive (dir </> "some.zip") $ do
            addFiles [path]
            names <- fileNames
            extractFiles names dir
