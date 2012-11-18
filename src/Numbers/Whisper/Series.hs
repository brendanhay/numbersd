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
    -- * Aliases
      Resolution
    , Step

    -- * Opaque
    , Series
    , create

    -- * Constants
    , maxResolution

    -- * Operations
    , toInterval
    , resolution
    , step
    , start
    , end
    , values
    , fetch
    , update
    ) where

import Data.Aeson
import Data.List     (intersperse)
import Data.Text     (pack)
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
    , points :: [Double]
    } deriving (Show)

maxResolution :: Resolution
maxResolution = 5 * 60

resolution :: Series -> Resolution
resolution = res

values :: Series -> [Double]
values = reverse . points

start :: Series -> Interval
start SS{..} = end - fromIntegral (length points * step)

instance Loggable Interval where
    build (I i) = build i

instance Loggable Series where
    build s@SS{..} = start s +++ "," +++ end +++ "," +++ step +++ "|" +++
        intersperse (build ",") (map build $ values s)

instance ToJSON Interval where
    toJSON (I i) = toJSON i

instance ToJSON Series where
    toJSON s@SS{..} = object
        [ pack "start"  .= start s
        , pack "end"    .= end
        , pack "step"   .= step
        , pack "values" .= values s
        ]

create :: Resolution -> Step -> Time -> Double -> Series
create r s ts val
    | r > maxResolution = error $ "Resolution to high: " ++ show r
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

replace :: Int -> Double -> [Double] -> [Double]
replace _ _ []  = []
replace n val (v:vs)
    | n == 0    = (val + v):vs
    | otherwise = v:replace (n - 1) val vs

singleton :: Int -> Double -> [Double]
singleton n = (: replicate n 0)

extend :: Int -> Double -> [Double] -> [Double]
extend n val = (singleton n val ++)
