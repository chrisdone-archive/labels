{-# LANGUAGE PackageImports #-}

module Main where

import           Control.Monad (forM_)
import qualified Data.ByteString.Lazy as B (hPut, pack, readFile)
import           Data.Word (Word8)
import           System.FilePath ((</>), (<.>))
import           System.IO (IOMode(..), hPutStr, withFile)
import           System.Directory (getCurrentDirectory, setCurrentDirectory)

import           Criterion.Main (Benchmark, bench, bgroup, defaultMain, nfIO)
import           System.IO.Temp (withSystemTempDirectory, withTempDirectory)
import           System.Random (getStdGen, randoms)

import qualified Codec.Archive.LibZip as L
import qualified "zip-archive" Codec.Archive.Zip as A
import           "zip-conduit" Codec.Archive.Zip
--import           Codec.Archive.Zip


main :: IO ()
main = do
    let sizes = [1024*1024, 10*1024*1024]    -- ^ sizes of files for benchmarking

    withSystemTempDirectory "zip-conduit" $ \dir -> do
        prepareFiles dir sizes
        defaultMain (prepareBench dir $ map show sizes)


-- | Prepares benchmarks.
prepareBench :: FilePath     -- ^ the path to the directory with files
             -> [FilePath]   -- ^ file names
             -> [Benchmark]
prepareBench dir names =
    [ bgroup "archive"
             [ bgroup "zip-conduit" $ b zipConduit
             , bgroup "zip-archive" $ b zipArchive
             , bgroup "libZip"      $ b libZip
             ]
    , bgroup "unarchive"
             [ bgroup "zip-conduit" $ b unZipConduit
             , bgroup "zip-archive" $ b unZipArchive
             -- , bgroup "libZip"      $ b unLibZip
             ]
    ]
  where
    b f = map (\name -> bench name $ nfIO $ f dir name) names


-- | Creates source files for archiving and archives with those
-- files. File name is the size of this file in bytes.
prepareFiles :: FilePath    -- ^ the path to the directory for files
             -> [Int]       -- ^ sizes of files to create
             -> IO ()
prepareFiles dir sizes = forM_ sizes $ \s -> do
    let path = dir </> show s

    createFile path s
    withArchive (path <.> "zip") $ addFiles [path]


-- | Creates a file of specified length with random content.
createFile :: FilePath -> Int -> IO ()
createFile path size =
    withFile path WriteMode $ \h -> do
        g <- getStdGen
        B.hPut h $ B.pack $ take size (randoms g :: [Word8])


------------------------------------------------------------------------------
-- Create zip archive with three different packages.

zipConduit :: FilePath -> FilePath -> IO ()
zipConduit dir name =
    withTempDirectory dir "zip-conduit" $ \tmpDir ->
        withArchive (tmpDir </> name <.> "zip") $ addFiles [dir </> name]


zipArchive :: FilePath -> FilePath -> IO ()
zipArchive dir name =
    withTempDirectory dir "zip-archive" $ \tmpDir -> do
        ar' <- A.addFilesToArchive [] A.emptyArchive [dir </> name]
        withFile (tmpDir </> name <.> "zip") WriteMode $ \h ->
            B.hPut h $ A.fromArchive ar'


libZip :: FilePath -> FilePath -> IO ()
libZip dir name =
    withTempDirectory dir "libZip" $ \tmpDir ->
        L.withArchive [L.CreateFlag] (tmpDir </> name <.> "zip") $ do
           zs <- L.sourceFile (dir </> name) 0 0
           L.addFile (dir </> name) zs
           return ()


------------------------------------------------------------------------------
-- Exctract files from archive with three different packages.

unZipConduit :: FilePath -> FilePath -> IO ()
unZipConduit dir name = do
    withArchive (dir </> name <.> "zip") $ do
        names <- entryNames
        extractFiles names $ dir -- </> "zip-conduit"


unZipArchive :: FilePath -> FilePath -> IO ()
unZipArchive dir name = do
    bytes <- B.readFile (dir </> name <.> "zip")
    withCurrentDirectory dir . A.extractFilesFromArchive [] $ A.toArchive bytes


unLibZip :: FilePath -> FilePath -> IO ()
unLibZip dir name = do
    bytes <- L.withArchive [] (dir </> name <.> "zip") $ L.fileContentsIx [] 0
    withFile (dir </> name) WriteMode $ \h ->
        hPutStr h bytes


------------------------------------------------------------------------------
-- Utils.

-- | Runs action in the specified current directory.
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path action = withSystemTempDirectory path $ \dir -> do
    current <- getCurrentDirectory
    setCurrentDirectory dir
    res <- action
    setCurrentDirectory current
    return res
