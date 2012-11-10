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

module Numbers.Whisper where

import Numbers.Types

import qualified Numbers.TMap as M

type Resolution = Integer
type Step = Integer

data Point = Point Time Double
    deriving (Show)

type Series = [Point]

data Whisper k = Whisper
    { _res  :: Resolution
    , _step :: Step
    , _tmap :: M.TMap k Series
    }

newWhisper :: Resolution -> Step -> IO (Whisper k)
newWhisper res step = Whisper res step `fmap` M.empty

point :: Ord k => k -> Point -> Whisper k -> IO ()
point key p Whisper{..} = M.update key (return . f) _tmap
  where
    f (Just ss) = insert _res _step p ss
    f Nothing   = [p]

series :: Ord k => k -> Whisper k -> IO (Maybe Series)
series key Whisper{..} = M.lookup key _tmap

-- select :: Resolution -> Step -> Time -> Time -> Series -> Series
-- select res step from to ss = take n $ filter (\(Point t _) -> t >= from) b
--   where
--     b = take n . tail $ balance step (Point to 0) ss
--     n = res `ceil` step

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

time :: Point -> Integer
time (Point (Time t) _) = t

generate :: Int -> Step -> Point -> [Point]
generate n step p = take n . map f . scanl1 (+) $ repeat step
  where
    t   = time p
    f s = Point (Time $ t - s) 0

diff :: Point -> Point -> Integer
diff x y = fromIntegral $ time x - time y

ceil :: Integral a => Integer -> Integer -> a
ceil x y = ceiling (fromIntegral x / fromIntegral y :: Double)
