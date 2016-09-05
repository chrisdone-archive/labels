{-# LANGUAGE CPP #-}

{- | Sink entries to the archive:

@
\{\-\# LANGUAGE OverloadedStrings \#\-\}

import qualified Data.Conduit.Binary as CB
import           Codec.Archive.Zip


main = do
    withArchive \"some.zip\" $ do
        sinkEntry \"first\"  $ CB.sourceLbs \"hello\"
        sinkEntry \"second\" $ CB.sourceLbs \"world\"
@

Source first entry from the archive:

@
import           System.Environment (getArgs)
import qualified Data.Conduit.Binary as CB
import           Codec.Archive.Zip

main = do
    archivePath:_ <- getArgs
    withArchive archivePath $ do
        name:_ <- entryNames
        sourceEntry name $ CB.sinkFile name
@

List entries in the archive:

@
import System.Environment (getArgs)
import Codec.Archive.Zip

main = do
    archivePath:_ <- getArgs
    names <- withArchive archivePath entryNames
    mapM_ putStrLn names
@


Add files to the archive:

@
import Control.Monad (filterM)
import System.Directory (doesFileExist, getDirectoryContents)
import System.Environment (getArgs)
import Codec.Archive.Zip

main = do
    dirPath:_ <- getArgs
    paths     <- getDirectoryContents dirPath
    filePaths <- filterM doesFileExist paths
    withArchive \"some.zip\" $ addFiles filePaths
@

Extract files from the archive:

@
import System.Environment (getArgs)
import Codec.Archive.Zip

main = do
    dirPath:_ <- getArgs
    withArchive \"some.zip\" $ do
        names <- entryNames
        extractFiles names dirPath
@

-}

module Codec.Archive.Zip
    ( -- * Archive monad
      Archive
    , withArchive

    -- * Operations
    , getComment
    , setComment
    , entryNames

    -- * Conduit interface
    , sourceEntry
    , sinkEntry
    , sinkEntryUncompressed

    -- * High level functions
    , extractFiles
    , addFiles
    , addFilesAs

    -- * Deprecated
    , fileNames
    , getSource
    , getSink
    ) where

import           Prelude hiding (readFile, zip)
import           Control.Applicative ((<$>))
import           Control.Monad (foldM, forM_)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B (empty)
import           Data.List (find)
import           Data.Maybe (fromMaybe)
import           Data.Time (UTCTime, getCurrentTime)
import           Data.Word (Word32)
import           System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime)
import           System.FilePath ((</>), dropDrive, takeDirectory)
import           System.IO (Handle, IOMode(..), SeekMode(..), hClose, hSeek, hTell, openFile, withFile)

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State (StateT, evalStateT, get, gets, modify, put)
import           Control.Monad.Trans.Resource (ResourceT, MonadResource, runResourceT)
import           Data.Conduit (Sink, Source, ($$), ($=), (=$))
import qualified Data.Conduit.Binary as CB (isolate, sinkFile, sinkHandle, sourceFile, sourceIOHandle)
import qualified Data.Conduit.List as CL (map)
import qualified Data.Conduit.Internal as CI (zipSinks)
import           Data.Conduit.Zlib (WindowBits(..), compress, decompress)

import           Codec.Archive.Zip.Internal
import           Codec.Archive.Zip.Util


------------------------------------------------------------------------------
-- Archive monad
type Archive = StateT Zip IO


data Zip = Zip
    { zipFilePath               :: FilePath
    , zipFileHeaders            :: [FileHeader]
    , zipCentralDirectoryOffset :: Int
    , zipComment                :: ByteString
    } deriving (Show)


withArchive :: FilePath -> Archive a -> IO a
withArchive path ar = do
    zip <- ifM (doesFileExist path)
               (readZip path)
               (return $ emptyZip path)

    evalStateT ar zip


readZip :: FilePath -> IO Zip
readZip f =
    withFile f ReadMode $ \h -> do
        e  <- readEnd h
        cd <- readCentralDirectory h e
        return Zip { zipFilePath    = f
                   , zipFileHeaders = cdFileHeaders cd
                   , zipCentralDirectoryOffset =
                         endCentralDirectoryOffset e
                   , zipComment     = endZipComment e
                   }


emptyZip :: FilePath -> Zip
emptyZip f = Zip { zipFilePath               = f
                 , zipFileHeaders            = []
                 , zipCentralDirectoryOffset = 0
                 , zipComment                = B.empty
                 }


------------------------------------------------------------------------------
-- Operations
entryNames :: Archive [FilePath]
entryNames = gets $ map fhFileName . zipFileHeaders


getComment :: Archive ByteString
getComment = gets zipComment


setComment :: ByteString -> Archive ()
setComment comment = modify $ \zip ->  zip { zipComment = comment }


------------------------------------------------------------------------------
-- Conduit interface
-- | Stream the contents of an archive entry to the specified sink.
sourceEntry :: FilePath -> Sink ByteString (ResourceT Archive) a -> Archive a
sourceEntry e sink = do
    zip <- get
    runResourceT $ sourceFile zip e $$ sink


-- | Stream data from the specified source to an archive entry.
sinkEntry :: FilePath -> Source (ResourceT Archive) ByteString -> Archive ()
sinkEntry e source = do
    zip  <- get
    time <- liftIO getCurrentTime
    zip' <- runResourceT $ source $$ sinkFile zip e Deflate time
    put zip'


-- | Stream data from the specified source to an uncompressed archive entry.
sinkEntryUncompressed :: FilePath -> Source (ResourceT Archive) ByteString -> Archive ()
sinkEntryUncompressed f source = do
    zip  <- get
    time <- liftIO getCurrentTime
    zip' <- runResourceT $ source $$ sinkFile zip f NoCompression time
    put zip'


sourceFile :: MonadResource m => Zip -> FilePath -> Source m ByteString
sourceFile zip f =
    source $= CB.isolate (fromIntegral $ fhCompressedSize fileHeader)
           $= decomp
  where
    source = CB.sourceIOHandle $ do
        h      <- openFile (zipFilePath zip) ReadMode
        offset <- calculateFileDataOffset h fileHeader
        hSeek h AbsoluteSeek offset
        return h

    fileHeader =
        fromMaybe (error "No such file.") $ find (\fh -> f == fhFileName fh)
                                          $ zipFileHeaders zip

    decomp =
        case fhCompressionMethod fileHeader of
          NoCompression -> CL.map id
          Deflate       -> decompress $ WindowBits (-15)


sinkFile :: MonadResource m => Zip -> FilePath -> CompressionMethod -> UTCTime
                            -> Sink ByteString m Zip
sinkFile zip f compression time = do
    h  <- liftIO $ openFile (zipFilePath zip) ReadWriteMode  -- not WriteMode because if the file already exists, then it would be truncated to zero length
    fh <- liftIO $ appendLocalFileHeader h zip f compression time
    dd <- sinkData h compression
    liftIO $ do
        writeDataDescriptorFields h dd offset
        let zip' = updateZip zip fh dd
        writeFinish h zip'
        hClose h
        return zip'
  where
    offset = fromIntegral $ zipCentralDirectoryOffset zip


------------------------------------------------------------------------------
-- High level functions

-- | Appends files to the 'Zip'. The file paths are used verbatim as zip entry
-- names, save for the application of 'dropDrive'.
addFiles :: [FilePath] -> Archive ()
addFiles = addFilesAs id

-- | Appends files to the 'Zip' using a function to transform the file paths
-- into zip entry names. Useful when dealing with absolute paths. 'dropDrive'
-- is applied to the paths before the supplied function.
addFilesAs :: (FilePath -> FilePath) -> [FilePath] -> Archive ()
addFilesAs funPath fs = do
    zip <- get
    zip' <- liftIO $ withFile (zipFilePath zip) ReadWriteMode $ \h -> do
        zip' <- foldM (addFile funPath h) zip fs
        writeFinish h zip'
        return zip'
    put zip'

-- | Extracts files from the 'Zip' to a directory.
extractFiles :: [FilePath] -> FilePath -> Archive ()
extractFiles fs dir = do
    zip <- get
    liftIO $ forM_ fs $ \fileName -> do
        createDirectoryIfMissing True $ dir </> takeDirectory fileName
        runResourceT $ sourceFile zip fileName $$ CB.sinkFile (dir </> fileName)


------------------------------------------------------------------------------
-- Low level functions

-- | Appends file to the 'Zip'.
addFile :: (FilePath -> FilePath) -> Handle -> Zip -> FilePath -> IO Zip
addFile funPath h zip f = do
#if MIN_VERSION_directory(1,2,0)
    m  <- getModificationTime f
#else
    m  <- clockTimeToUTCTime <$> getModificationTime f
#endif
    fh <- appendLocalFileHeader h zip (funPath $ dropDrive f) Deflate m
    dd <- runResourceT $ CB.sourceFile f $$ sinkData h Deflate
    writeDataDescriptorFields h dd offset
    return $ updateZip zip fh dd
  where
    offset = fromIntegral $ zipCentralDirectoryOffset zip


appendLocalFileHeader :: Handle -> Zip -> FilePath -> CompressionMethod
                      -> UTCTime -> IO FileHeader
appendLocalFileHeader h zip f compression time = do
    hSeek h AbsoluteSeek offset
    writeLocalFileHeader h fh
    return fh
  where
    offset = fromIntegral $ zipCentralDirectoryOffset zip
    fh     = mkFileHeader f compression time (fromIntegral offset)


mkFileHeader :: FilePath -> CompressionMethod -> UTCTime -> Word32 -> FileHeader
mkFileHeader f compression lastModified relativeOffset =
    FileHeader { fhBitFlag                = 2  -- max compression for deflate compression method
               , fhCompressionMethod      = compression
               , fhLastModified           = lastModified
               , fhCRC32                  = 0
               , fhCompressedSize         = 0
               , fhUncompressedSize       = 0
               , fhInternalFileAttributes = 0
               , fhExternalFileAttributes = 0
               , fhRelativeOffset         = relativeOffset
               , fhFileName               = f
               , fhExtraField             = B.empty
               , fhFileComment            = B.empty
               }


sinkData :: MonadResource m
         => Handle -> CompressionMethod -> Sink ByteString m DataDescriptor
sinkData h compression = do
    ((uncompressedSize, crc32), compressedSize) <-
        case compression of
          NoCompression -> CI.zipSinks sizeCrc32Sink sizeDataSink
          Deflate       -> CI.zipSinks sizeCrc32Sink compressSink
    return DataDescriptor
               { ddCRC32            = crc32
               , ddCompressedSize   = fromIntegral compressedSize
               , ddUncompressedSize = fromIntegral uncompressedSize
               }
  where
    compressSink :: MonadResource m => Sink ByteString m Int
    compressSink = compress 6 (WindowBits (-15)) =$ sizeDataSink

    sizeCrc32Sink :: MonadResource m => Sink ByteString m (Int, Word32)
    sizeCrc32Sink =  CI.zipSinks sizeSink crc32Sink

    sizeDataSink :: MonadResource m => Sink ByteString m Int
    sizeDataSink  = fst <$> CI.zipSinks sizeSink (CB.sinkHandle h)


-- Writes data descriptor fields (crc-32, compressed size and
-- uncompressed size) in the middle of the local file header.
writeDataDescriptorFields :: Handle -> DataDescriptor -> Integer -> IO ()
writeDataDescriptorFields h dd offset = do
    old <- hTell h
    hSeek h AbsoluteSeek $ offset + 4 + 2 + 2 + 2 + 2 + 2
    writeDataDescriptor h dd
    hSeek h AbsoluteSeek old


updateZip :: Zip -> FileHeader -> DataDescriptor -> Zip
updateZip zip fh dd =
    zip { zipFileHeaders = zipFileHeaders zip
                        ++ [ fh { fhCRC32            = ddCRC32 dd
                                , fhCompressedSize   = ddCompressedSize dd
                                , fhUncompressedSize = ddUncompressedSize dd
                                } ]
       , zipCentralDirectoryOffset = zipCentralDirectoryOffset zip
                                   + fromIntegral (localFileHeaderLength fh + ddCompressedSize dd)
       }


writeFinish :: Handle -> Zip -> IO ()
writeFinish h zip = do
    writeCentralDirectory h $ CentralDirectory (zipFileHeaders zip)
    writeEnd h
             (length $ zipFileHeaders zip)                      -- total number of entries in the central directory on this disk
             (sum $ map fileHeaderLength $ zipFileHeaders zip)
             (zipCentralDirectoryOffset zip)


------------------------------------------------------------------------------
-- Deprecated
fileNames :: Archive [FilePath]
fileNames = entryNames


getSource :: MonadResource m => FilePath -> Archive (Source m ByteString)
getSource f = gets $ \zip -> sourceFile zip f


getSink :: MonadResource m
        => FilePath -> UTCTime -> Archive (Sink ByteString m ())
getSink f time = gets $ \zip -> do
                            sinkFile zip f Deflate time
                            return ()
