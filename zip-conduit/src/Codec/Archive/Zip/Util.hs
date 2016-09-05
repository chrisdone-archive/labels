module Codec.Archive.Zip.Util where

import           Control.Applicative ((<$>))
import           Data.Bits ((.&.), shiftR, shiftL)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B (length)
import           Data.Time (UTCTime(..), TimeOfDay(..), fromGregorian, picosecondsToDiffTime, secondsToDiffTime, timeToTimeOfDay, toGregorian)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Word (Word16, Word32)
import           System.Time (ClockTime(..))

import           Data.Conduit (Sink)
import qualified Data.Conduit.List as CL (fold)
import           Data.Digest.CRC32 (crc32Update)
import           Data.Serialize.Get (Get, getWord32le, isEmpty, lookAhead, runGet, skip)


ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond conseq altern = do
    c <- cond
    if c then conseq else altern


many :: (Monad m, Functor m) => m (Maybe a) -> m [a]
many p = do
  r <- p
  case r of
       Just x  -> (x:) <$> many p
       Nothing -> return []


------------------------------------------------------------------------------
-- Serialize utils.
maybeEmpty :: Get a -> Get (Maybe a)
maybeEmpty p = do
    e <- isEmpty
    if e
      then return Nothing
      else Just <$> p


runGet' :: Get a -> ByteString -> a
runGet' g b =
    either error id $ runGet g b


signature :: Word32 -> Get ()
signature sig = do
    s <- lookAhead getWord32le
    if s == sig
      then skip 4
      else fail "Wrong signature."


------------------------------------------------------------------------------
-- Time utils.
data MSDOSDateTime = MSDOSDateTime
    { msDOSDate :: Word16
    , msDOSTime :: Word16
    } deriving (Show)


msDOSDateTimeToUTCTime :: MSDOSDateTime -> UTCTime
msDOSDateTimeToUTCTime dosDateTime =
    UTCTime { utctDay = fromGregorian year month day
            , utctDayTime =
                secondsToDiffTime $ hours * 60 * 60 + minutes * 60 + seconds
            }
  where
    seconds = fromIntegral $ 2 * (dosTime .&. 0x1F)
    minutes = fromIntegral $ (shiftR dosTime 5) .&. 0x3F
    hours   = fromIntegral $ shiftR dosTime 11

    day     = fromIntegral $ dosDate .&. 0x1F
    month   = fromIntegral $ (shiftR dosDate 5) .&. 0xF
    year    = 1980 + fromIntegral (shiftR dosDate 9)

    dosDate = msDOSDate dosDateTime
    dosTime = msDOSTime dosDateTime


utcTimeToMSDOSDateTime :: UTCTime -> MSDOSDateTime
utcTimeToMSDOSDateTime utcTime =
    MSDOSDateTime { msDOSDate = dosDate
                  , msDOSTime = dosTime
                  }
  where
    dosTime = fromIntegral $ seconds + shiftL minutes 5 + shiftL hours 11
    dosDate = fromIntegral $ day + shiftL month 5 + shiftL year 9

    seconds = fromEnum (todSec tod) `div` 2
    minutes = todMin tod
    hours   = todHour tod
    tod     = timeToTimeOfDay $ utctDayTime utcTime

    year    = fromIntegral year' - 1980
    (year', month, day) = toGregorian $ utctDay utcTime


clockTimeToUTCTime :: ClockTime -> UTCTime
clockTimeToUTCTime (TOD seconds picoseconds) =
    let utcTime = posixSecondsToUTCTime $ fromIntegral seconds in
    utcTime { utctDayTime = utctDayTime utcTime
                          + picosecondsToDiffTime picoseconds
            }


------------------------------------------------------------------------------
-- Conduit utils.
crc32Sink :: Monad m => Sink ByteString m Word32
crc32Sink =
    CL.fold crc32Update 0


sizeSink :: Monad m => Sink ByteString m Int
sizeSink =
    CL.fold (\acc input -> B.length input + acc) 0
