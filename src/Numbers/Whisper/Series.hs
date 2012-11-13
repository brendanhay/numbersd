{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Numbers.Whisper.Series
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Whisper.Series (
    -- * Opaque
      Series
    , create

    -- * Operations
    , fetch
    , update
    ) where

import Blaze.ByteString.Builder (Builder, copyLazyByteString)
import Data.Aeson
import Data.Aeson.Types


import Data.Lens.Common
import Data.Lens.Template
import Numeric                  (showFFloat)
import Data.Foldable            (toList)
import Data.List                (intercalate, intersperse)
import Data.Maybe
import Data.Sequence            (Seq, (<|), (|>), (><))
import Data.Text                (pack)
import Data.Text.Encoding       (decodeUtf8)
import Numbers.Types
import Data.Foldable


import qualified Data.Attoparsec.Number as A

import qualified Data.Vector as V
import qualified Data.Sequence as S

newtype Interval = I Int
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

type Points = Seq Double

data Series = SS
    { _len    :: Int
    , _step   :: Int
    , _end    :: Interval
    , _points :: Points
    } deriving (Show)

-- $(makeLens ''Series)

instance Loggable Interval where
    build (I i) = build i

instance Loggable Points where
    build = build . intercalate "," . map f . toList
       where
         f v = showFFloat (Just 1) v ""

instance Loggable Series where
    build s@SS{..} = (start s) +++ "," +++ _end +++ "," +++ _step
                         +++ "|" +++ _points

instance ToJSON Interval where
    toJSON (I i) = toJSON i

instance ToJSON Points where
    toJSON = Array . V.fromList . map (Number . A.D) . toList

instance ToJSON Series where
    toJSON s@SS{..} = object
        [ pack "start"  .= start s
        , pack "end"    .= _end
        , pack "step"   .= _step
        , pack "values" .= _points
        ]

create :: Int -> Int -> Time -> Double -> Series
create len step ts = SS len step end . S.singleton
  where
    end = (toInterval step ts) + fromIntegral step

-- | TODO: Workout 'from' time slices
fetch :: Time -> Time -> Series -> Series
fetch _ to s@SS{..} = append (toInterval _step to) 0 s

update :: Time -> Double -> Series -> Series
update ts val s@SS{..} = append (toInterval _step ts) val s

append :: Interval -> Double -> Series -> Series
append to val s@SS{..} = s
    { _points = S.take (_len - 1) p
    , _end    = _end + (fromIntegral $ e * _step)
    }
  where
    d = distance _step (start s) to
    (e, p) | d < S.length _points = (0, insert (d + 1) val _points)
           | d >= _len            = (_len, val <| empty (_len - 1))
           | otherwise            = (d, val <| (empty (d - 1) >< _points))

start :: Series -> Interval
start SS{..} = _end - (fromIntegral $ S.length _points * _step)

distance :: Int -> Interval -> Interval -> Int
distance step from to = abs $ ceiling diff
  where
    diff = fromIntegral (to - from) / fromIntegral step :: Double

insert :: Int -> Double -> Points -> Points
insert idx val = S.adjust (+ val) idx

empty :: Int -> Points
empty n = S.replicate n 0

toInterval :: Int -> Time -> Interval
toInterval step (Time t) = I $ t - (t `mod` step)

-- def file_fetch(fh, fromTime, untilTime):
--   header = __readHeader(fh)
--   now = int( time.time() )
--   if untilTime is None:
--     untilTime = now
--   fromTime = int(fromTime)
--   untilTime = int(untilTime)

--   # Here we try and be flexible and return as much data as we can.
--   # If the range of data is from too far in the past or fully in the future, we
--   # return nothing
--   if (fromTime > untilTime):
--     raise InvalidTimeInterval("Invalid time interval: from time '%s' is after until time '%s'" % (fromTime, untilTime))

--   oldestTime = now - header['maxRetention']
--   # Range is in the future
--   if fromTime > now:
--     return None
--   # Range is beyond retention
--   if untilTime < oldestTime:
--     return None
--   # Range requested is partially beyond retention, adjust
--   if fromTime < oldestTime:
--     fromTime = oldestTime
--   # Range is partially in the future, adjust
--   if untilTime > now:
--     untilTime = now

--   diff = now - fromTime
--   for archive in header['archives']:
--     if archive['retention'] >= diff:
--       break

--   fromInterval = int( fromTime - (fromTime % archive['secondsPerPoint']) ) + archive['secondsPerPoint']
--   untilInterval = int( untilTime - (untilTime % archive['secondsPerPoint']) ) + archive['secondsPerPoint']
--   fh.seek(archive['offset'])
--   packedPoint = fh.read(pointSize)
--   (baseInterval,baseValue) = struct.unpack(pointFormat,packedPoint)

--   if baseInterval == 0:
--     step = archive['secondsPerPoint']
--     points = (untilInterval - fromInterval) / step
--     timeInfo = (fromInterval,untilInterval,step)
--     valueList = [None] * points
--     return (timeInfo,valueList)

--   #Determine fromOffset
--   timeDistance = fromInterval - baseInterval
--   pointDistance = timeDistance / archive['secondsPerPoint']
--   byteDistance = pointDistance * pointSize
--   fromOffset = archive['offset'] + (byteDistance % archive['size'])

--   #Determine untilOffset
--   timeDistance = untilInterval - baseInterval
--   pointDistance = timeDistance / archive['secondsPerPoint']
--   byteDistance = pointDistance * pointSize
--   untilOffset = archive['offset'] + (byteDistance % archive['size'])

--   #Read all the points in the interval
--   fh.seek(fromOffset)
--   if fromOffset < untilOffset: #If we don't wrap around the archive
--     seriesString = fh.read(untilOffset - fromOffset)
--   else: #We do wrap around the archive, so we need two reads
--     archiveEnd = archive['offset'] + archive['size']
--     seriesString = fh.read(archiveEnd - fromOffset)
--     fh.seek(archive['offset'])
--     seriesString += fh.read(untilOffset - archive['offset'])

--   #Now we unpack the series data we just read (anything faster than unpack?)
--   byteOrder,pointTypes = pointFormat[0],pointFormat[1:]
--   points = len(seriesString) / pointSize
--   seriesFormat = byteOrder + (pointTypes * points)
--   unpackedSeries = struct.unpack(seriesFormat, seriesString)

--   #And finally we construct a list of values (optimize this!)
--   valueList = [None] * points #pre-allocate entire list for speed
--   currentInterval = fromInterval
--   step = archive['secondsPerPoint']

--   for i in xrange(0,len(unpackedSeries),2):
--     pointTime = unpackedSeries[i]
--     if pointTime == currentInterval:
--       pointValue = unpackedSeries[i+1]
--       valueList[i/2] = pointValue #in-place reassignment is faster than append()
--     currentInterval += step

--   fh.close()
--   timeInfo = (fromInterval,untilInterval,step)
--   return (timeInfo,valueList)
