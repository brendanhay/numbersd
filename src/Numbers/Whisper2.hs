{-# LANGUAGE TemplateHaskell, TupleSections #-}

-- |
-- Module      : Numbers.Whisper2
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Whisper2 (
    -- * Exported Types
      Range
    , Step
    , Point(..)

    -- * Opaque
    , Whisper
    , newWhisper

    -- * Insertion
    , addMetric

    -- * Serialisation
    , textSeries
    , jsonSeries
    ) where

import Blaze.ByteString.Builder (Builder, copyLazyByteString)
import Data.Aeson
import Data.Lens.Common
import Data.Lens.Template
import Data.Maybe
import Data.Monoid
import Data.Sequence (Seq, (<|), (|>), (><))
import Numbers.Types

import Numeric                  (showFFloat)
import Data.List                (intercalate)
import Data.Text                (pack)
import Data.Text.Encoding       (decodeUtf8)
import Data.Attoparsec.Number   (Number(D))
import Data.Foldable (toList)

import qualified Data.Map      as M
import qualified Data.Sequence as S

type Range = Integer
type Step  = Integer

data Res = Res Range Step

data Point = Point Time Double

data Series = Series Time (Seq Double) deriving (Show)

data Whisper = Whisper
    { _series :: M.Map Key Series
    , _res    :: Res
    }

newWhisper :: Range -> Step -> Whisper
newWhisper r s = Whisper M.empty $ Res r s

instance Loggable Series where
    build (Series t s) = build . intercalate "," . map stringify . reverse $ toList s

instance ToJSON Series where
    toJSON = toJSON . show


addMetric :: Key -> Metric -> Time -> Whisper -> Whisper
addMetric key val ts w = createPoint w key (Point ts v)
  where
    v = case val of
        (Counter d) -> d
        (Timer ds)  -> sum ds / fromIntegral (length ds)
        (Gauge d)   -> d
        (Set _)     -> 1


textSeries :: Time -> Whisper -> Builder
textSeries to w@Whisper{..} = build . mconcat . map f $ allSeries to w
  where
    (Res _ _step) = _res
    f (k, s) = k +++ "," +++ start s _step +++ ","
        +++ end s +++ "," +++ _step +++ "|" +++ build s +++ "\n"

jsonSeries :: Time -> Whisper -> Builder
jsonSeries to w@Whisper{..} = enc . map f $ allSeries to w
  where
    (Res _ _step) = _res
    enc = copyLazyByteString . encode . object
    f (Key k, s) = decodeUtf8 k .= object
        [ pack "start"  .= start s _step
        , pack "end"    .= end s
        , pack "step"   .= _step
        , pack "points" .= toJSON s
        ]

allSeries :: Time -> Whisper -> [(Key, Series)]
allSeries to w@Whisper{..} = mapMaybe f keys
  where
    f k  = (k,) `fmap` getSeries k to w
    keys = M.keys _series
           
getSeries :: Key -> Time -> Whisper -> Maybe Series
getSeries key to Whisper{..} =
    case M.lookup key _series of
        Just x  -> Just $ prependSeries x _res (Point to 0)
        Nothing -> Nothing



createPoint :: Whisper -> Key -> Point -> Whisper
createPoint w@Whisper{..} key pnt =
    w { _series = M.alter (Just . f) key _series }
  where
    f (Just x) = prependSeries x _res pnt
    f Nothing  = createSeries pnt

createSeries :: Point -> Series
createSeries (Point t v) = Series t $ S.singleton v

prependSeries :: Series -> Res -> Point -> Series
prependSeries (Series x vs) r@(Res m s) p@(Point y v) = f
  where
        -- If the point is erroneous, just prepend it and increment the time
    f | y <= x    = Series (next x r) $ v <| vs
        -- If the allocated length is greater or equal to the maximum resolution
        -- just throw the old series away
      | cells >= max' = createSeries p
      | otherwise = Series y q

    -- Otherwise, generate the empty cell sequence and prepend the new point
    q = S.take max' $ v <| (S.replicate (cells - 1) 0) >< vs

    -- The maximum resolution
    max'  = fromIntegral $ m `ceil` s -- pass this in precalced

    -- The number of empty cells to allocate
    cells = fromIntegral (y - x) `ceil` s

next :: Time -> Res -> Time
next (Time t) (Res _ s) = Time $ t + s

ceil :: Integer -> Integer -> Int
ceil x y = ceiling (fromIntegral x / fromIntegral y :: Double)

end :: Series -> Integer
end (Series t _) = toInteger t

start :: Series -> Step -> Integer
start (Series t vs) s = toInteger $ (fromIntegral t) - (S.length vs) * 60

stringify v = showFFloat (Just 1) v ""

time :: Point -> Integer
time (Point (Time t) _) = t

