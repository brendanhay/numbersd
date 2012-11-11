{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Numbers.Whisper
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Whisper (
    -- * Exported Types
      Resolution
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
import Data.Maybe
import Data.Monoid
import Numeric                  (showFFloat)
import Data.List                (intercalate)
import Numbers.Types
import Data.Text                (pack)
import Data.Text.Encoding       (decodeUtf8)
import Data.Attoparsec.Number   (Number(D))

import qualified Data.Map as M

type Resolution = Integer
type Step       = Integer

data Point = Point Time Double
    deriving (Show)

type Series = [Point]

data Whisper = Whisper
    { _tmap :: M.Map Key Series
    , _res  :: Resolution
    , _step :: Step
    }

instance Loggable Series where
    build = build . intercalate "," . map stringify

instance ToJSON Point where
    toJSON = Number . D . read . stringify

newWhisper :: Resolution -> Step -> Whisper
newWhisper = Whisper M.empty

addMetric :: Key -> Metric -> Time -> Whisper -> Whisper
addMetric key val ts = addPoint key (Point ts v)
  where
    v = case val of
        (Counter d) -> d
        (Timer ds)  -> sum ds / fromIntegral (length ds)
        (Gauge d)   -> d
        (Set _)     -> 1

addPoint :: Key -> Point -> Whisper -> Whisper
addPoint key p w@Whisper{..} = w { _tmap = M.alter (return . f) key _tmap }
  where
    f = insert _res _step p . fromMaybe []

textSeries :: Time -> Whisper -> Builder
textSeries to w@Whisper{..} = build . mconcat . map f $ allSeries to w
  where
    f (k, s) = k +++ "," +++ start s +++ ","
        +++ end s +++ "," +++ _step +++ "|" +++ build s +++ "\n"

jsonSeries :: Time -> Whisper -> Builder
jsonSeries to w@Whisper{..} = enc . map f $ allSeries to w
  where
    enc = copyLazyByteString . encode . object
    f (Key k, s) = decodeUtf8 k .= object
        [ pack "start"  .= start s
        , pack "end"    .= end s
        , pack "step"   .= _step
        , pack "points" .= toJSON s
        ]

allSeries :: Time -> Whisper -> [(Key, Series)]
allSeries to w@Whisper{..} = mapMaybe f keys
  where
    f k  = (k,) `fmap` series k to w
    keys = M.keys _tmap

series :: Key -> Time -> Whisper -> Maybe Series
series key to Whisper{..} = (take n . balance _step (point to)) `fmap` ss
  where
    n  = _res `ceil` _step
    ss = M.lookup key _tmap

insert :: Resolution -> Step -> Point -> Series -> Series
insert res step p ss = take n $ balance step p ss
  where
    n = res `ceil` step

balance :: Step -> Point -> Series -> Series
balance step x []             = x:gen
  where
    gen = map (\s -> point . Time $ time x + s) . scanl1 (-) $ repeat step
balance step x ss | d <= step = x:ss
                  | otherwise = x:a ++ ss
  where
    a = take (c - 1) $ generate step (-) x
    c = d `ceil` step
    d = x `diff` head ss

generate :: Step -> (Integer -> Integer -> Integer) -> Point -> [Point]
generate step op p = map f . scanl1 (+) $ repeat step
  where
    f = point . Time . op (time p)

end :: Series -> Integer
end = time . head

start :: Series -> Integer
start = time . last

diff :: Point -> Point -> Integer
diff x y = fromIntegral $ time x - time y

time :: Point -> Integer
time (Point (Time t) _) = t

point :: Time -> Point
point = flip Point 0

stringify :: Point -> String
stringify (Point _ v) = showFFloat (Just 1) v ""

ceil :: Integral a => Integer -> Integer -> a
ceil x y = ceiling (fromIntegral x / fromIntegral y :: Double)
