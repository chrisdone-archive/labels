{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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
  , dropConduit
  , mapConduit
  , foldSink
  , countSink
  , groupConduit
  , groupByConduit
  , explodeConduit
  , filterConduit
  , sinkConduit
  , projectConduit
  , tableSink
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
  , Csv
  , csv
  , vectorCsvConduit
    -- * Reading Zip files
  , zipEntryConduit
    -- * Module re-exports
  , module Labels
  , module Data.Time
  , module Data.Ord
  , module Data.Function
  ) where

import           Codec.Archive.Zip
import           Control.Monad.Catch
import           Control.Monad.Error
import           Control.Monad.Reader
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Char
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CL
import           Data.Csv hiding (Csv)
import           Data.Csv.Conduit
import           Data.Function
import qualified Data.HashMap.Strict as M
import           Data.List
import           Data.List.Split
import           Data.Ord
import           Data.Proxy
import           Data.Text (Text)
import           Data.Time
import           Data.Vector (Vector)
import           GHC.TypeLits
import           Labels
import           Labels.CSV () -- Bring in instances.
import           Labels.Cassava.Instances
import           Network.HTTP.Client.Conduit hiding (Proxy)
import           Network.HTTP.Simple hiding (Proxy)
import           System.Directory
import           System.IO
import           Text.Printf

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

instance FromField Day where
  parseField xs = parseTimeM True defaultTimeLocale "%F" (S8.unpack xs)

instance ToField Day where
  toField xs = toField (formatTime defaultTimeLocale "%F" xs)

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
{-# INLINE takeConduit #-}
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
{-# INLINE mapConduit #-}
mapConduit :: Monad m => (a -> b) -> Conduit a m b
mapConduit = CL.map

-- | Fold over the source inputs and return a final value.
{-# INLINE foldSink #-}
foldSink
  :: Monad m
  => (b -> a -> b) -> b -> ConduitM a o m b
foldSink = CL.fold

-- | Count all the inputs.
{-# INLINE countSink #-}
countSink :: (Monad m) => Consumer a m Int
countSink = foldSink (\x _ -> x + 1) 0

{-# INLINE filterConduit #-}
filterConduit :: Monad m => (a -> Bool) -> Conduit a m a
filterConduit = CL.filter

sinkConduit :: (Monad m) => Consumer a m result -> Conduit a m result
sinkConduit m = do
  v <- m
  yield v

{-# INCLUDE dropConduit #-}
dropConduit :: Monad m => Int -> Conduit a m a
dropConduit 0 = awaitForever yield
dropConduit n = do
  m <- await
  case m of
    Nothing -> return ()
    Just x -> do
      yield x
      dropConduit (n - 1)

{-# INCLUDE groupByConduit #-}
groupByConduit :: Monad m => (a -> a -> Bool) -> Conduit a m [a]
groupByConduit = CL.groupBy


{-# INCLUDE explodeConduit #-}
explodeConduit :: (Foldable f, Monad m) => Conduit (f a) m a
explodeConduit = CL.concat

--------------------------------------------------------------------------------
-- Conduits

-- | Open a file and yield the contents as a source of byte chunks.
{-# INLINE fileSource #-}
fileSource :: MonadIO m
  => FilePath -> Producer (ExploreT m) ByteString
fileSource fp = do
  h <- liftIO (openFile fp ReadMode)
  sourceHandle h
  liftIO (hClose h)
  -- FIXME: Use MonadResource to free the handle properly.  This is a
  -- resource leak.

-- | Open a file and write the input stream into it.
{-# INLINE fileSink #-}
fileSink :: MonadIO m
  => FilePath -> Consumer ByteString (ExploreT m) ()
fileSink fp = do
  h <- liftIO (openFile fp WriteMode)
  sinkHandle h
  liftIO (hClose h)
  -- FIXME: Use MonadResource to free the handle properly.  This is a
  -- resource leak.

-- | Write the stream to stdout.
{-# INLINE printSink #-}
printSink :: (MonadIO m,Show a)
  => Consumer a (ExploreT m) ()
printSink = awaitForever (liftIO . print)

-- | Write the stream to stdout.
{-# INLINE stdoutSink #-}
stdoutSink :: MonadIO m
  => Consumer ByteString (ExploreT m) ()
stdoutSink = do
  sinkHandle stdout
  -- FIXME: Use MonadResource to free the handle properly.  This is a
  -- resource leak.

-- | Output stats about the input.
{-# INLINE statSink #-}
statSink :: MonadIO m => Consumer ByteString (ExploreT m) ()
statSink = do
  size <- CL.fold (\total bytes -> S.length bytes + total) 0
  liftIO (putStrLn ("Bytes: " ++ commas size))
  where
    commas = reverse . intercalate "," . chunksOf 3 . reverse . show

-- | Make a request and from the reply yield a source of byte chunks.
{-# INLINE httpSource #-}
httpSource :: MonadIO m
  => Request -> Producer (ExploreT m) ByteString
httpSource req = do
  resp <- lift (ExploreT (responseOpen req))
  getResponseBody resp

-- | CSV configuration.
type Csv a = ("rowType" := Proxy a, "downcase" := Bool, "seperator" := Char)

-- | Default CSV configuration.
csv :: Csv a
csv = (#rowType := Proxy, #downcase := False, #seperator := ',')

-- | Read input bytes and yield rows of columns, return typed
-- determined polymorphically.
{-# INLINE fromCsvConduit #-}
fromCsvConduit
  :: forall a m.
     (MonadCatch m, FromNamedRecord a)
  => Csv a -> Conduit ByteString (ExploreT m) a
fromCsvConduit config =
  if get #downcase config
    then fromNamedCsv options $= CL.map unDowncaseColumns
    else fromNamedCsv options
  where
    options =
      defaultDecodeOptions
      {decDelimiter = fromIntegral (ord (get #seperator config))}

-- | Read input bytes and yield rows of columns, each row is a vector
-- of columns.
{-# INLINE vectorCsvConduit #-}
vectorCsvConduit :: (MonadCatch m)
  => Conduit ByteString (ExploreT m) (Vector Text)
vectorCsvConduit = fromCsv defaultDecodeOptions NoHeader

-- | Treat the input as a zip archive, extract the given entry and
-- yield byte chunks from it.
{-# INLINE zipEntryConduit #-}
zipEntryConduit :: MonadIO m
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

--------------------------------------------------------------------------------
-- Record things

{-# INCLUDE groupConduit #-}
groupConduit
  :: (Has label a record, Monad m, Eq a)
  => Proxy label -> Conduit record m [record]
groupConduit field = CL.groupBy (on (==) (get field))

-- | Project a subset of fields from the input record.
projectConduit :: forall to from m. (Monad m, Project from to) => Conduit from m to
projectConduit = CL.map project

instance (ToField t1) =>
         ToNamedRecord (l1 := t1) where
  toNamedRecord (l1 := v1) =
    M.fromList [(key l1, toField v1)]
    where
      key = S8.pack . symbolVal

instance (ToField t1,ToField t2) =>
         ToNamedRecord (l1 := t1, l2 := t2) where
  toNamedRecord (l1 := v1, l2 := v2) =
    M.fromList [(key l1, toField v1), (key l2, toField v2)]
    where
      key = S8.pack . symbolVal

instance (ToField t1,ToField t2,ToField t3) =>
         ToNamedRecord (l1 := t1, l2 := t2, l3 := t3) where
  toNamedRecord (l1 := v1, l2 := v2,l3 := v3) =
    M.fromList [(key l1, toField v1), (key l2, toField v2)
               ,(key l3, toField v3)]
    where
      key = S8.pack . symbolVal

instance (ToField t1, ToField t2, ToField t3, ToField t4) =>
         ToNamedRecord (l1 := t1, l2 := t2, l3 := t3, l4 := t4) where
  toNamedRecord (l1 := v1, l2 := v2, l3 := v3, l4 := v4) =
    M.fromList
      [ (key l1, toField v1)
      , (key l2, toField v2)
      , (key l3, toField v3)
      , (key l4, toField v4)
      ]
    where
      key = S8.pack . symbolVal

-- | Sink all results into a table and print to stdout.
tableSink :: (ToNamedRecord record,MonadIO m) => Consumer record (ExploreT m) ()
tableSink = do
  rows <- CL.map toNamedRecord $= CL.consume
  case rows of
    [] -> return ()
    _ ->
      liftIO
        (putStrLn
           (tablize
              (map
                 (map ((True, ) . S8.unpack))
                 (M.keys (Data.List.head rows) : map M.elems rows))))

-- | Make a table out of a list of rows.
tablize :: [[(Bool,String)]] -> String
tablize xs =
  intercalate "\n"
              (map (intercalate "  " . map fill . zip [0 ..]) xs)
  where fill (x',(left,text')) = printf ("%" ++ direction ++ show width ++ "s") text'
          where direction = if left
                               then "-"
                               else ""
                width = maximum (map (length . snd . (!! x')) xs)
