{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Numbers.Whisper.Sequence
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Whisper.Sequence where

import Prelude hiding (replicate)
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
create len step ts = SS len step end . singleton len
  where
    end = (toInterval step ts) -- + fromIntegral step

-- | TODO: Workout 'from' time slices
fetch :: Time -> Time -> Series -> Series
fetch _ to s@SS{..} = append (toInterval _step to) 0 s

-- Why does 'to' not align in the web api?
-- shouldn't all the to fields be exactly the same?

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
           | d >= _len            = (_len, singleton (_len - 1) val)
           | otherwise            = (d, extend (d - 1) val _points)

start :: Series -> Interval
start SS{..} = _end - (fromIntegral $ S.length _points * _step)

distance :: Int -> Interval -> Interval -> Int
distance step from to = abs $ ceiling diff
  where
    diff = fromIntegral (to - from) / fromIntegral step :: Double

insert :: Int -> Double -> Points -> Points
insert n val = S.adjust (+ val) n

extend :: Int -> Double -> Points -> Points
extend n val = (singleton n val ><)

singleton :: Int -> Double -> Points
singleton n val = val <| replicate n

replicate :: Int -> Points
replicate n = S.replicate n 0

toInterval :: Int -> Time -> Interval
toInterval step (Time t) = I $ t - (t `mod` step)
