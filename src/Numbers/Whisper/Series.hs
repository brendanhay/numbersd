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
    -- * Types
      Resolution
    , Step
    , Time(..)
    , Interval(..)

    , Series
    , resolution
    , start
    , end
    , step
    , values
    , datapoints

    -- * Constants
    , maxResolution

    -- * Series operations
    , create
    , fetch
    , update
    ) where

import Data.Aeson
import Data.List
import Data.Maybe
import Numbers.Types

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
    , points :: [Maybe Double]
    } deriving (Eq, Show)

maxResolution :: Resolution
maxResolution = 5 * 60

resolution :: Series -> Resolution
resolution = res

values :: Series -> [Maybe Double]
values = reverse . points

start :: Series -> Interval
start SS{..} = end - fromIntegral (length points * step)

datapoints :: Series -> [(Interval, Maybe Double)]
datapoints s = zip (timeline (end s) (res s) (step s)) (values s)

timeline :: Interval -> Resolution -> Step -> [Interval]
timeline t r s =
  take r $ iterate (decrementInterval s) t

decrementInterval :: Step -> Interval -> Interval
decrementInterval s (I t) = I (t - s)

instance Loggable Interval where
    build (I i) = build i

instance Loggable Series where
    build s@SS{..} = start s &&& "," <&& end &&& "," <&& step &&& "|"
      <&& intersperse (sbuild ",") (map (maybe (build noneStr) build) $ values s)

noneStr :: String
noneStr = "None"

instance ToJSON Interval where
    toJSON (I i) = toJSON i

create :: Resolution -> Step -> Time -> Double -> Series
create r s ts val
    | r > maxResolution = error $ "Resolution too high: " ++ show r
    | otherwise         = SS r s (toInterval s ts) (singleton (r - 1) val)

fetch :: Time -> Time -> Series -> Series
fetch _ to s@SS{..} = append (toInterval step to) 0 s

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

replace :: Int -> Double -> [Maybe Double] -> [Maybe Double]
replace _ _ []  = []
replace n val (v:vs)
    | n == 0    = (Just $ val + fromMaybe 0 v):vs
    | otherwise = v:replace (n - 1) val vs

singleton :: Int -> Double -> [Maybe Double]
singleton n x = (Just x) : replicate n Nothing

extend :: Int -> Double -> [Maybe Double] -> [Maybe Double]
extend n val = (singleton n val ++)
