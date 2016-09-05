module Codec.Archive.Zip.Internal where

import           Prelude hiding (readFile)
import           Control.Applicative ((<$>))
import           Control.Monad (unless)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B (hGet, hPut, length, pack, empty)
import           Data.Serialize (Get, Put, getByteString, getWord16le, getWord32le, putByteString, putWord16le, putWord32le, runPut, skip)
import           Data.Time (UTCTime)
import           Data.Word (Word16, Word32)
import           System.IO (Handle, SeekMode(..), hFileSize, hSeek, hTell)

import           Data.ByteString.UTF8 (fromString, toString)

import           Codec.Archive.Zip.Util


calculateFileDataOffset :: Handle -> FileHeader -> IO Integer
calculateFileDataOffset h fh = do
    lfhLength <- readLocalFileHeaderLength h fh
    return $ fromIntegral (fhRelativeOffset fh) + lfhLength


------------------------------------------------------------------------------
-- Overall zipfile format:
--   [local file header + file data + data_descriptor] . . .
--   [central directory] end of central directory record


------------------------------------------------------------------------------
-- Local file header:
--
-- local file header signature     4 bytes  (0x04034b50)
-- version needed to extract       2 bytes
-- general purpose bit flag        2 bytes
-- compression method              2 bytes
-- last mod file time              2 bytes
-- last mod file date              2 bytes
-- crc-32                          4 bytes
-- compressed size                 4 bytes
-- uncompressed size               4 bytes
-- file name length                2 bytes
-- extra field length              2 bytes
--
-- file name (variable size)
-- extra field (variable size)

localFileHeaderConstantLength :: Int
localFileHeaderConstantLength = 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2


readLocalFileHeaderLength :: Handle -> FileHeader -> IO Integer
readLocalFileHeaderLength h header =
    runGet' getLocalFileHeaderLength <$> hGetLocalFileHeader h header


-- Gets length of the local file header, i.e. sum of lengths of its
-- constant and variable parts.
getLocalFileHeaderLength :: Get Integer
getLocalFileHeaderLength = do
    signature 0x04034b50
    skip $ 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4
    fileNameLength    <- fromIntegral <$> getWord16le
    extraFieldLength  <- fromIntegral <$> getWord16le

    return $ fromIntegral localFileHeaderConstantLength
           + fileNameLength
           + extraFieldLength


writeLocalFileHeader :: Handle -> FileHeader -> IO ()
writeLocalFileHeader h fh =
    B.hPut h . runPut $ putLocalFileHeader fh


putLocalFileHeader :: FileHeader -> Put
putLocalFileHeader fh = do
    putWord32le 0x04034b50
    putWord16le 20  -- version needed to extract (>= 2.0)
    putWord16le $ fhBitFlag fh
    putWord16le compressionMethod
    putWord16le $ msDOSTime modTime
    putWord16le $ msDOSDate modTime
    putWord32le $ fhCRC32 fh
    putWord32le $ fhCompressedSize fh
    putWord32le $ fhUncompressedSize fh
    putWord16le . fromIntegral . B.length . fromString $ fhFileName fh
    putWord16le . fromIntegral . B.length $ fhExtraField fh
    putByteString . fromString $ fhFileName fh
    putByteString $ fhExtraField fh
  where
    modTime = utcTimeToMSDOSDateTime $ fhLastModified fh
    compressionMethod = case fhCompressionMethod fh of
                          NoCompression -> 0
                          Deflate       -> 8


-- Gets constant part of the local file header.
hGetLocalFileHeader :: Handle -> FileHeader -> IO ByteString
hGetLocalFileHeader h fh = do
    hSeek h AbsoluteSeek offset
    B.hGet h localFileHeaderConstantLength
  where
    offset = fromIntegral $ fhRelativeOffset fh


localFileHeaderLength :: FileHeader -> Word32
localFileHeaderLength fh =
  fromIntegral $ 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2
               + length (fhFileName fh) + B.length (fhExtraField fh)


------------------------------------------------------------------------------
-- Data descriptor
--
-- crc-32                          4 bytes
-- compressed size                 4 bytes
-- uncompressed size               4 bytes
data DataDescriptor = DataDescriptor
    { ddCRC32            :: Word32
    , ddCompressedSize   :: Word32
    , ddUncompressedSize :: Word32
    } deriving (Show)


writeDataDescriptor :: Handle -> DataDescriptor -> IO ()
writeDataDescriptor h dd =
    B.hPut h . runPut $ putDataDescriptor dd


putDataDescriptor :: DataDescriptor -> Put
putDataDescriptor dd = do
--    putWord32le 0x08074b50
    putWord32le $ ddCRC32 dd
    putWord32le $ ddCompressedSize dd
    putWord32le $ ddUncompressedSize dd


------------------------------------------------------------------------------
-- Central directory structure:
--
-- [file header 1]
-- ...
-- [file header n]

data CentralDirectory = CentralDirectory
    { cdFileHeaders      :: [FileHeader]
    } deriving (Show)


readCentralDirectory :: Handle -> End -> IO CentralDirectory
readCentralDirectory h e =
    runGet' getCentralDirectory <$> hGetCentralDirectory h e


writeCentralDirectory :: Handle -> CentralDirectory -> IO ()
writeCentralDirectory h cd =
    B.hPut h . runPut $ putCentralDirectory cd


putCentralDirectory :: CentralDirectory -> Put
putCentralDirectory cd =
    mapM_ putFileHeader $ cdFileHeaders cd


getCentralDirectory :: Get CentralDirectory
getCentralDirectory = do
    headers <- many . maybeEmpty $ getFileHeader
    return CentralDirectory { cdFileHeaders = headers }


hGetCentralDirectory :: Handle -> End -> IO ByteString
hGetCentralDirectory h e = do
    hSeek h AbsoluteSeek $ fromIntegral offset
    B.hGet h size
  where
    size   = endCentralDirectorySize e
    offset = endCentralDirectoryOffset e


------------------------------------------------------------------------------
-- File header:
--
-- central file header signature   4 bytes  (0x02014b50)
-- version made by                 2 bytes
-- version needed to extract       2 bytes
-- general purpose bit flag        2 bytes
-- compression method              2 bytes
-- last mod file time              2 bytes
-- last mod file date              2 bytes
-- crc-32                          4 bytes
-- compressed size                 4 bytes
-- uncompressed size               4 bytes
-- file name length                2 bytes
-- extra field length              2 bytes
-- file comment length             2 bytes
-- disk number start               2 bytes
-- internal file attributes        2 bytes
-- external file attributes        4 bytes
-- relative offset of local header 4 bytes

-- file name (variable size)
-- extra field (variable size)
-- file comment (variable size)

data FileHeader = FileHeader
    { fhBitFlag                :: Word16
    , fhCompressionMethod      :: CompressionMethod
    , fhLastModified           :: UTCTime
    , fhCRC32                  :: Word32
    , fhCompressedSize         :: Word32
    , fhUncompressedSize       :: Word32
    , fhInternalFileAttributes :: Word16
    , fhExternalFileAttributes :: Word32
    , fhRelativeOffset         :: Word32
    , fhFileName               :: FilePath
    , fhExtraField             :: ByteString
    , fhFileComment            :: ByteString
    } deriving (Show)


data CompressionMethod = NoCompression
                       | Deflate
                         deriving (Show)


getFileHeader :: Get FileHeader
getFileHeader = do
    signature 0x02014b50
    skip 2
    versionNeededToExtract <- getWord16le
    unless (versionNeededToExtract <= 20) $
        fail "This archive requires zip >= 2.0 to extract."
    bitFlag                <- getWord16le
    rawCompressionMethod   <- getWord16le
    compessionMethod       <- case rawCompressionMethod of
                                0 -> return NoCompression
                                8 -> return Deflate
                                _ -> fail $ "Unknown compression method "
                                          ++ show rawCompressionMethod
    lastModFileTime        <- getWord16le
    lastModFileDate        <- getWord16le
    crc32                  <- getWord32le
    compressedSize         <- fromIntegral <$> getWord32le
    uncompressedSize       <- getWord32le
    fileNameLength         <- fromIntegral <$> getWord16le
    extraFieldLength       <- fromIntegral <$> getWord16le
    fileCommentLength      <- fromIntegral <$> getWord16le
    skip 2
    internalFileAttributes <- getWord16le
    externalFileAttributes <- getWord32le
    relativeOffset         <- fromIntegral <$> getWord32le
    fileName               <- getByteString fileNameLength
    extraField             <- getByteString extraFieldLength
    fileComment            <- getByteString fileCommentLength
    return FileHeader
               { fhBitFlag                = bitFlag
               , fhCompressionMethod      = compessionMethod
               , fhLastModified           = toUTC lastModFileDate lastModFileTime
               , fhCRC32                  = crc32
               , fhCompressedSize         = compressedSize
               , fhUncompressedSize       = uncompressedSize
               , fhInternalFileAttributes = internalFileAttributes
               , fhExternalFileAttributes = externalFileAttributes
               , fhRelativeOffset         = relativeOffset
               , fhFileName               = toString fileName
               , fhExtraField             = extraField
               , fhFileComment            = fileComment
               }
  where
    toUTC date time =
        msDOSDateTimeToUTCTime MSDOSDateTime { msDOSDate = date
                                             , msDOSTime = time
                                             }


putFileHeader :: FileHeader -> Put
putFileHeader fh = do
    putWord32le 0x02014b50
    putWord16le 0   -- version made by
    putWord16le 20  -- version needed to extract (>= 2.0)
    putWord16le $ fhBitFlag fh
    putWord16le compressionMethod
    putWord16le $ msDOSTime modTime
    putWord16le $ msDOSDate modTime
    putWord32le $ fhCRC32 fh
    putWord32le $ fhCompressedSize fh
    putWord32le $ fhUncompressedSize fh
    putWord16le . fromIntegral . B.length . fromString $ fhFileName fh
    putWord16le . fromIntegral . B.length $ fhExtraField fh
    putWord16le . fromIntegral . B.length $ fhFileComment fh
    putWord16le 0  -- disk number start
    putWord16le $ fhInternalFileAttributes fh
    putWord32le $ fhExternalFileAttributes fh
    putWord32le $ fhRelativeOffset fh
    putByteString . fromString $ fhFileName fh
    putByteString $ fhExtraField fh
    putByteString $ fhFileComment fh
  where
    modTime = utcTimeToMSDOSDateTime $ fhLastModified fh
    compressionMethod = case fhCompressionMethod fh of
                          NoCompression -> 0
                          Deflate       -> 8


fileHeaderLength :: FileHeader -> Word32
fileHeaderLength fh =
  fromIntegral $ 4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4
               + length (fhFileName fh) + B.length (fhExtraField fh)
               + B.length (fhFileComment fh)


------------------------------------------------------------------------------
-- End of central directory record:
--
-- end of central dir signature    4 bytes  (0x06054b50)
-- number of this disk             2 bytes
-- number of the disk with the
-- start of the central directory  2 bytes
-- total number of entries in the
-- central directory on this disk  2 bytes
-- total number of entries in
-- the central directory           2 bytes
-- size of the central directory   4 bytes
-- offset of start of central
-- directory with respect to
-- the starting disk number        4 bytes
-- .ZIP file comment length        2 bytes
-- .ZIP file comment       (variable size)

data End = End
    { endCentralDirectorySize   :: Int
    , endCentralDirectoryOffset :: Int
    , endZipComment             :: ByteString
    } deriving (Show)


readEnd :: Handle -> IO End
readEnd h =
    runGet' getEnd <$> hGetEnd h


getEnd :: Get End
getEnd = do
   skip $ 2 + 2 + 2 + 2
   size          <- fromIntegral <$> getWord32le
   offset        <- fromIntegral <$> getWord32le
   commentLength <- fromIntegral <$> getWord16le
   comment       <- getByteString commentLength
   return End { endCentralDirectorySize   = size
              , endCentralDirectoryOffset = offset
              , endZipComment             = comment
              }


-- TODO: find a better way to find the end of central dir signature
hGetEnd :: Handle -> IO ByteString
hGetEnd h = do
    hSeek h SeekFromEnd (-4)
    loop
  where
    loop = do
        s <- B.hGet h 4

        if s == B.pack (reverse [0x06, 0x05, 0x4b, 0x50])
          then get
          else next

    get = do
        size   <- hFileSize h
        offset <- hTell h
        B.hGet h $ fromIntegral (size - offset)

    next = do
        hSeek h RelativeSeek (-5)
        loop


writeEnd :: Handle -> Int -> Word32 -> Int -> IO ()
writeEnd h number size offset =
     B.hPut h . runPut $ putEnd number size offset


putEnd :: Int -> Word32 -> Int -> Put
putEnd number size offset = do
    putWord32le 0x06054b50
    putWord16le 0                      -- disk number
    putWord16le 0                      -- disk number of central directory
    putWord16le $ fromIntegral number  -- number of entries this disk
    putWord16le $ fromIntegral number  -- number of entries
    putWord32le size                   -- size of central directory
    putWord32le $ fromIntegral offset  -- offset of central dir
    -- TODO: put comment
    putWord16le 0
    putByteString B.empty
