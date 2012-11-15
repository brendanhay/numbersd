{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Numbers.Whisper.List
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Whisper.List (
    -- * Opaque
      Series
    , create

    , Interval
    , toInterval

    -- * Operations
    , resolution
    , step
    , start
    , end
    , points
    , fetch
    , update
    ) where

import Blaze.ByteString.Builder (Builder, copyLazyByteString)
import Data.Aeson
import Data.Aeson.Types
import Numeric                  (showFFloat)
import Data.Foldable            (toList)
import Data.List                (intercalate, intersperse)
import Data.Maybe
import Data.Text                (pack)
import Data.Text.Encoding       (decodeUtf8)
import Numbers.Types

import qualified Data.Attoparsec.Number as A
import qualified Data.Vector as V

type Resolution = Int
type Step       = Int

newtype Interval = I Int
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

toInterval :: Step -> Time -> Interval
toInterval s (Time t) = I $ t - (t `mod` s)

data Series = SS
    { res    :: Resolution
    , step   :: Step
    , end    :: Interval
    , points :: [Double]
    } deriving (Show)

resolution :: Series -> Resolution
resolution = res

start :: Series -> Interval
start SS{..} = end - fromIntegral (length points * step)

instance Loggable Interval where
    build (I i) = build i

-- instance Loggable [Double] where
--     build = build . intercalate "," . map f . toList
--        where
--          f v = showFFloat (Just 1) v ""

instance Loggable Series where
    build s@SS{..} = start s +++ "," +++ end +++ "," +++ step
                         +++ "|" +++ points

instance ToJSON Interval where
    toJSON (I i) = toJSON i

instance ToJSON Series where
    toJSON s@SS{..} = object
        [ pack "start"  .= start s
        , pack "end"    .= end
        , pack "step"   .= step
        , pack "values" .= points
        ]

create :: Resolution -> Step -> Time -> Double -> Series
create r s ts val | r > 640   = error $ "Resolution to high: " ++ show r
                  | otherwise = SS r s e ps
  where
    e  = toInterval s ts
    ps = singleton (r - 1) val

-- | TODO: Workout 'from' time slices
fetch :: Time -> Time -> Series -> Series
fetch _ to s@SS{..} = append (toInterval step to) 0 s

-- Why does 'to' not align in the web api?
-- shouldn't all the to fields be exactly the same?

update :: Time -> Double -> Series -> Series
update ts val s@SS{..} = append (toInterval step ts) val s

append :: Interval -> Double -> Series -> Series
append to val s@SS{..} = s { points = take res p, end = e }
  where
    d = distance step end to
    (e, p) | to <= end = (end, replace d val points)
           | d  >= res = (to,  singleton (res - 1) val)
           | otherwise = (to,  extend (d - 1) val points)

distance :: Step -> Interval -> Interval -> Int
distance s from to = abs $ ceiling diff
  where
    diff = fromIntegral (abs to - abs from) / fromIntegral s :: Double

extend :: Int -> Double -> [Double] -> [Double]
extend n val = (singleton n val ++)

replace :: Int -> Double -> [Double] -> [Double]
replace _ _ []  = []
replace n val (v:vs)
    | n == 0    = (val + v):vs
    | otherwise = v:replace (n - 1) val vs

singleton :: Int -> Double -> [Double]
singleton n = (: replicate n 0)
