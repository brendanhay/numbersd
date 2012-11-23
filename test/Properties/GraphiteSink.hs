{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Properties.GraphiteSink
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Properties.GraphiteSink (graphiteProperties) where

import Control.Applicative        hiding (empty)
import Data.Attoparsec.Combinator        (many1)
import Data.Maybe
import Numbers.Sink.Graphite
import Numbers.Types
import Properties.Generators             ()
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.Attoparsec.Char8 as PC
import qualified Data.ByteString.Char8 as BS

graphiteProperties :: Test
graphiteProperties = testGroup "graphite sink"
    [ testProperty "encodes counter value and value per second" prop_encodes_counter_values
    ]

-- prop_format :: Key -> Metric -> Time -> Int -> Bool
-- prop_format key val ts n =
--     [E key (read $ flatten val) ts] == parse (encode key val ts n)

-- prop_encodes_counter_value :: Key -> Double -> Time -> Int -> Bool
-- prop_encodes_counter_value key val ts n =
--     [E key val ts] ==  parse (encode key (Counter val) ts n)

prop_encodes_counter_values :: Key -> Double -> Time -> Int -> Bool
prop_encodes_counter_values key@(Key k) val ts n =
    [E k1 vs ts, E k2 val ts] ==  parse (encode key (Counter val) ts n)
  where
    k1 = Key $ "counters" `BS.append` k
    k2 = Key $ "counters.count" `BS.append` k
    vs = val / (fromIntegral n / 1000)

-- prop_encodes_timer_mean_percentile

-- prop_encodes_timer_upper_percentile

-- prop_encodes_timer_sum_percentile

-- prop_encodes_timer_std_value

-- prop_encodes_timer_upper_value

-- prop_encodes_timer_lower_value

-- prop_encodes_timer_count_value

-- prop_encodes_timer_sum_value

-- prop_encodes_timer_mean_value

-- prop_encodes_gauge_value

-- prop_encodes_set_count_value


data Encoded = E Key Double Time
    deriving (Show)

instance Eq Encoded where
    (E k1 v1 t1) == (E k2 v2 t2) = k1 == k2 && vals && t1 == t2
      where
        vals = abs (v1 - v2) < 0.005

parse :: BS.ByteString -> [Encoded]
parse = fromJust . decode (many1 format)
  where
    format = do
        k <- PC.takeTill (== ' ') <* PC.char ' '
        v <- PC.double <* PC.char ' '
        t <- PC.decimal
        return $ E (Key k) v (Time t)
