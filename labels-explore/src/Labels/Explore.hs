{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-deprecations #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Handy exploration functionality.

-- Intended to be imported like:
--
-- @import qualified Labels.Explore as X@

module Labels.Explore
  ( -- * Starting point
    explore
  , ExploreT
    -- * Conduit combinators
  , (.|)
  , (.>)
  , takeConduit
  , mapConduit
    -- * Printing things to the console
  , stdoutSink
  , statSink
  , printSink
    -- * Reading and writing files
  , fileSource
  , fileSink
    -- * Making HTTP requests
  , httpSource
    -- * Reading CSV data
  , fromCsvConduit
  , vectorCsvConduit
    -- * Reading Zip files
  , zipEntryConduit
    -- * Module re-exports
  , module Labels
  ) where

import           Codec.Archive.Zip
import           Control.Monad.Catch
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Data.Csv
import           Data.Csv.Conduit
import           Data.List
import           Data.List.Split
import           Data.Text (Text)
import           Data.Vector (Vector)
import           Labels
import           Labels.CSV () -- Bring in instances.
import           Network.HTTP.Client.Conduit
import           Network.HTTP.Simple
import           System.Directory
import           System.IO

--------------------------------------------------------------------------------
-- Types

-- | Context for exploring data.
newtype ExploreT m a = ExploreT
  { runExploreT :: ReaderT Manager m a
  } deriving ( MonadIO, Monad, Applicative, Functor, MonadThrow
             , MonadCatch)

instance (MonadThrow m, MonadCatch m) =>
         MonadError CsvParseError (ExploreT m) where
  throwError = throwM
  catchError = catch

instance Exception CsvParseError

--------------------------------------------------------------------------------
-- Starting point

-- | Explore some data.
explore
  :: MonadIO m
  => ExploreT m a -> m a
explore m = withManager (runExploreT m)

--------------------------------------------------------------------------------
-- Conduit combinators

-- | @x .| y@ pipes x into y, like a regular shell pipe.
(.|) :: Monad m => Conduit a m b -> ConduitM b c m r -> ConduitM a c m r
(.|) = (=$=)

-- | @x .> y@ writes the stream x into the sink y. This is like
-- writing the final output of a UNIX pipe to a file.
(.>) :: Monad m => Source m a -> Sink a m b -> m b
(.>) = ($$)

-- | Take n items from the stream.
takeConduit :: Monad m => Int -> Conduit a m a
takeConduit 0 = return ()
takeConduit n = do
  v <- await
  case v of
    Nothing -> return ()
    Just x -> do
      yield x
      takeConduit (n - 1)

-- | Map over a conduit.
mapConduit :: Monad m => (a -> b) -> Conduit a m b
mapConduit = CL.map

--------------------------------------------------------------------------------
-- Conduits

-- | Open a file and yield the contents as a source of byte chunks.
fileSource
  :: MonadIO m
  => FilePath -> Producer (ExploreT m) ByteString
fileSource fp = do
  h <- liftIO (openFile fp ReadMode)
  sourceHandle h
  liftIO (hClose h)
  -- FIXME: Use MonadResource to free the handle properly.  This is a
  -- resource leak.

-- | Open a file and write the input stream into it.
fileSink
  :: MonadIO m
  => FilePath -> Consumer ByteString (ExploreT m) ()
fileSink fp = do
  h <- liftIO (openFile fp WriteMode)
  sinkHandle h
  liftIO (hClose h)
  -- FIXME: Use MonadResource to free the handle properly.  This is a
  -- resource leak.

-- | Write the stream to stdout.
printSink
  :: (MonadIO m,Show a)
  => Consumer a (ExploreT m) ()
printSink = awaitForever (liftIO . print)

-- | Write the stream to stdout.
stdoutSink
  :: MonadIO m
  => Consumer ByteString (ExploreT m) ()
stdoutSink = do
  sinkHandle stdout
  -- FIXME: Use MonadResource to free the handle properly.  This is a
  -- resource leak.

-- | Output stats about the input.
statSink :: MonadIO m => Consumer ByteString (ExploreT m) ()
statSink = do
  size <- CL.fold (\total bytes -> S.length bytes + total) 0
  liftIO (putStrLn ("Bytes: " ++ commas size))
  where
    commas = reverse . intercalate "," . chunksOf 3 . reverse . show

-- | Make a request and from the reply yield a source of byte chunks.
httpSource
  :: MonadIO m
  => Request -> Producer (ExploreT m) ByteString
httpSource req = do
  resp <- lift (ExploreT (responseOpen req))
  getResponseBody resp

-- | Read input bytes and yield rows of columns, return typed
-- determined polymorphically.
fromCsvConduit
  :: (MonadCatch m,FromNamedRecord a)
  => Conduit ByteString (ExploreT m) a
fromCsvConduit = fromNamedCsv defaultDecodeOptions

-- | Read input bytes and yield rows of columns, each row is a vector
-- of columns.
vectorCsvConduit
  :: (MonadCatch m)
  => Conduit ByteString (ExploreT m) (Vector Text)
vectorCsvConduit = fromCsv defaultDecodeOptions NoHeader

-- | Treat the input as a zip archive, extract the given entry and
-- yield byte chunks from it.
zipEntryConduit
  :: MonadIO m
  => String -> Conduit ByteString (ExploreT m) ByteString
zipEntryConduit name = do
  fileSink archivePath
  liftIO (withArchive archivePath (sourceEntry name (sinkFile fp)))
  fileSource fp
  liftIO (removeFile fp) -- FIXME: use a temp directory. This is garbage, but for demo purposes, it's allowed.
  liftIO (removeFile archivePath)
  where
    fp = name ++ ".tmp"
    archivePath = name ++ ".zip"
