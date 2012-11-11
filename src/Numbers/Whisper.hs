{-# LANGUAGE TemplateHaskell, TupleSections #-}

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
    , addPoint

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
import Data.Attoparsec.Number   (Number(D))

import qualified Data.Map as M

type Resolution = Integer
type Step       = Integer

data Point = Point Time Double
    deriving (Show)

type Series = [Point]

data Whisper k = Whisper
    { _res  :: Resolution
    , _step :: Step
    , _tmap :: M.Map k Series
    }

instance Loggable Series where
    build = build . intercalate "," . map stringify

instance ToJSON Point where
    toJSON = Number . D . read . stringify

newWhisper :: Resolution -> Step -> Whisper k
newWhisper res step = Whisper res step M.empty

addPoint :: Ord k => k -> Point -> Whisper k -> Whisper k
addPoint key p w@Whisper{..} = w { _tmap = M.alter (return . f) key _tmap }
  where
    f (Just ss) = insert _res _step p ss
    f Nothing   = [p]

textSeries :: (Ord k, Loggable k) => Time -> Whisper k -> Builder
textSeries to w@Whisper{..} = build . mconcat . map f $ allSeries to w
  where
    f (k, s) = k +++ "," +++ start s +++ ","
        +++ end s +++ "," +++ _step +++ "|" +++ build s +++ "\n"

jsonSeries :: (Ord k, ToJSON k) => Time -> Whisper k -> Builder
jsonSeries to w@Whisper{..} =
    copyLazyByteString . encode . object . map f $ allSeries to w
  where
    t k = let (String b) = toJSON k
          in b
    f (k, s) = t k .= object
        [ pack "start"  .= start s
        , pack "end"    .= end s
        , pack "step"   .= _step
        , pack "points" .= toJSON s
        ]

allSeries :: Ord k => Time -> Whisper k -> [(k, Series)]
allSeries to w@Whisper{..} = catMaybes $ map f keys
  where
    f k  = (k,) `fmap` series k to w
    keys = M.keys _tmap

series :: Ord k => k -> Time -> Whisper k -> Maybe Series
series key to Whisper{..} = select _res _step to `fmap` ss
  where
    ss = M.lookup key _tmap

select :: Resolution -> Step -> Time -> Series -> Series
select res step to ss = take n b
  where
    b = take n . tail $ balance step (Point to 0) ss
    n = res `ceil` step

insert :: Resolution -> Step -> Point -> Series -> Series
insert res step p ss = take n $ balance step p ss
  where
    n = res `ceil` step

balance :: Step -> Point -> Series -> Series
balance _    x []                   = [x]
balance step x ss@(y:_) | d <= step = x:ss
                        | otherwise = x:a ++ ss
  where
    a = generate (c - 1) step x
    c = d `ceil` step
    d = x `diff` y

generate :: Int -> Step -> Point -> [Point]
generate n step p = take n . map f . scanl1 (+) $ repeat step
  where
    t   = time p
    f s = Point (Time $ t - s) 0

diff :: Point -> Point -> Integer
diff x y = fromIntegral $ time x - time y

ceil :: Integral a => Integer -> Integer -> a
ceil x y = ceiling (fromIntegral x / fromIntegral y :: Double)

end :: Series -> Integer
end = time . head

start :: Series -> Integer
start = time . last

time :: Point -> Integer
time (Point (Time t) _) = t

stringify :: Point -> String
stringify (Point _ v) = showFFloat (Just 1) v ""
