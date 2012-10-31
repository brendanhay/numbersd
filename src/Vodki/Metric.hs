{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      : Vodki.Metric
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Vodki.Metric (
    -- * Type Classes
      Metric(..)
    , Encode(..)

    -- * Exported Types
    , Key(..)
    , Counter(..)
    , Gauge(..)
    , Timer(..)
    , Set(..)
    ) where

import Control.Applicative        hiding (empty)
import Control.Monad                     (liftM)
import Data.Attoparsec.ByteString
import Data.Monoid                       (mappend)
import Data.Time.Clock.POSIX
import Vodki.Sink
import Vodki.Regex

import qualified Data.Attoparsec.Char8 as PC
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set              as S

class Show a => Metric a where
    empty  :: a
    append :: a -> a -> a
    parser :: Parser (Key, a)

    decode :: BS.ByteString -> Maybe (Key, a)
    decode s = maybeResult $ feed (parse parser s) ""

class Show b => Encode a b | b -> a where
    encode :: Key -> b -> POSIXTime -> Int -> BS.ByteString
    encode k v ts n = BS.intercalate ", " $ map BS.pack
        [ show k
        , show v
        , show ts
        , show n
        ]

newtype Key = Key BS.ByteString
    deriving (Eq, Ord, Show)

newtype Counter = Counter Double
    deriving (Eq, Ord, Show)

instance Metric Counter where
    empty = Counter 0
    append (Counter a) (Counter b) = Counter $! a + b
    parser = do
        k <- getKey
        v <- getVal
        _ <- getType 'c'
        r <- getRate
        return (k, Counter $! maybe v (sample v) r)
      where
        sample n v = v * (1 / n)

instance Encode Console Counter

newtype Timer = Timer [Double]
    deriving (Eq, Ord, Show)

instance Metric Timer where
    parser = getMetric 'm' (Timer . (:[]))
    empty  = Timer []
    append (Timer a) (Timer b) = Timer $! a `mappend` b

instance Encode Console Timer

newtype Gauge = Gauge Double
    deriving (Eq, Ord, Show)

instance Metric Gauge where
    parser     = getMetric 'g' Gauge
    empty      = Gauge 0
    append _ b = b

instance Encode Console Gauge

newtype Set = Set (S.Set Double)
    deriving (Eq, Ord, Show)

instance Metric Set where
    parser = getMetric 's' (Set . S.singleton)
    empty  = Set S.empty
    append (Set a) (Set b) = Set $! a `mappend` b

instance Encode Console Set

getMetric :: Char -> (Double -> a) -> Parser (Key, a)
getMetric c f = mapTup f `liftM` ((,) <$> getKey <*> getVal <* getType c)

getKey :: Parser Key
getKey = Key . strip <$> PC.takeTill (== ':') <* PC.char ':'
  where
    strip s = foldl (flip replace) s unsafe'

getVal :: Parser Double
getVal = PC.double <* PC.char '|'

getType :: Char -> Parser Char
getType = PC.char

getRate :: Parser (Maybe Double)
getRate = optional (PC.char '|' *> PC.char '@' *> PC.double >>= fail . show)

mapTup :: (b -> c) -> (a, b) -> (a, c)
mapTup f (a, b) = (a, f b)

