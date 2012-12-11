{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Properties.Conduit
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Properties.Conduit (
      conduitProperties
    ) where

import Control.Applicative                  hiding (empty)
import Data.Maybe
import Numbers.Conduit
import Numbers.Types
import Properties.Generators
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.Attoparsec.Char8 as PC
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set              as S
import qualified Data.Vector           as V

conduitProperties :: Test
conduitProperties = testGroup "sinks"
    [ testGroup "graphite"
        [ testGroup "aggregate event"
            [ testProperty "encodes prefix" prop_graphite_aggr_event_encodes_prefix
            , testProperty "encodes key" prop_graphite_aggr_event_encodes_key
            , testProperty "encodes value" prop_graphite_aggr_event_encodes_value
            ]
        , testProperty "ignores non aggregate events" prop_graphite_ignores_non_aggr_events
        ]
    , testGroup "broadcast"
        [ testProperty "doesn't modify received packets" prop_broadcast_doesnt_modify_received_packets
        , testProperty "ignores non receive events" prop_broadcast_ignores_non_receive_events
        ]
    , testGroup "downstream"
        [ testGroup "flush event"
            [ testProperty "encodes key" prop_downstream_flush_event_encodes_key
            , testProperty "aggregates values" prop_downstream_flush_event_aggregates_values
            , testProperty "encodes multiple values per line" prop_downstream_flush_event_encodes_mvalues_per_line
            ]
        , testProperty "ignores non flush events" prop_downstream_ignores_non_flush_events
        ]
    ]

prop_graphite_aggr_event_encodes_prefix :: Graphite -> Bool
prop_graphite_aggr_event_encodes_prefix g =
    inputGPrefix g == outputGPrefix g

prop_graphite_aggr_event_encodes_key :: Graphite -> Bool
prop_graphite_aggr_event_encodes_key g =
    inputGKey g == outputGKey g

prop_graphite_aggr_event_encodes_value :: Graphite -> Bool
prop_graphite_aggr_event_encodes_value g =
    inputGValue g `kindaClose` outputGValue g

prop_graphite_ignores_non_aggr_events :: Property
prop_graphite_ignores_non_aggr_events =
    forAll (graphite "" `conduitP` p) null
  where
    p Aggregate{} = False
    p _           = True

prop_broadcast_doesnt_modify_received_packets :: Broadcast -> Bool
prop_broadcast_doesnt_modify_received_packets (Broadcast s bs) =
    [s] == bs

prop_broadcast_ignores_non_receive_events :: Property
prop_broadcast_ignores_non_receive_events =
    forAll (broadcast `conduitP` p) null
  where
    p Receive{} = False
    p _         = True

prop_downstream_flush_event_encodes_key :: Downstream -> Bool
prop_downstream_flush_event_encodes_key d =
    inputDKey d == outputDKey d

prop_downstream_flush_event_aggregates_values :: Downstream -> Bool
prop_downstream_flush_event_aggregates_values d =
    inputDMetric d `kindaCloseM` outputDMetric d

prop_downstream_flush_event_encodes_mvalues_per_line :: Downstream -> Bool
prop_downstream_flush_event_encodes_mvalues_per_line d =
    len (inputDMetric d) == ':' `BS.count` inputDEncoded d
  where
    len (Timer vs) = V.length vs
    len (Set   vs) = S.size vs
    len _          = 1

prop_downstream_ignores_non_flush_events :: Property
prop_downstream_ignores_non_flush_events =
    forAll (downstream `conduitP` p) null
  where
    p Flush{} = False
    p _       = True

data Downstream = Downstream
    { inputDKey     :: Key
    , inputDMetric  :: Metric
    , inputDEncoded :: BS.ByteString
    , outputDKey    :: Key
    , outputDMetric :: Metric
    } deriving (Show)

downstreamP :: (Metric -> Bool) -> Gen Downstream
downstreamP p = do
    ik <- arbitrary
    im <- arbitrary `suchThat` p
    it <- arbitrary
    r  <- BS.intercalate "\n" <$> conduitResult [Flush ik im it] downstream
    let (ok, om) = fromMaybe ("failed", Counter 0) $ decode lineParser r
    return Downstream
        { inputDKey     = ik
        , inputDMetric  = im
        , inputDEncoded = r
        , outputDKey    = ok
        , outputDMetric = om
        }

instance Arbitrary Downstream where
    arbitrary = downstreamP $ const True

conduitP :: EventConduit Gen BS.ByteString -> (Event -> Bool) -> Gen [BS.ByteString]
conduitP con p = do
    e <- arbitrary `suchThat` p
    conduitResult [e] con

data Graphite = Graphite
    { inputGPrefix  :: String
    , inputGKey     :: Key
    , inputGTime    :: Time
    , inputGValue   :: Double
    , outputGPrefix :: String
    , outputGKey    :: Key
    , outputGTime   :: Time
    , outputGValue  :: Double
    } deriving (Show)

instance Arbitrary Graphite where
    arbitrary = do
        SafeStr ip  <- arbitrary
        it          <- arbitrary
        p@(P ik iv) <- arbitrary
        bs          <- conduitResult [Aggregate p it] (graphite ip)
        let (op, ok, ot, ov) = parseGraphite bs
        return Graphite
            { inputGPrefix  = ip
            , inputGKey     = ik
            , inputGTime    = it
            , inputGValue   = iv
            , outputGPrefix = op
            , outputGKey    = ok
            , outputGTime   = ot
            , outputGValue  = ov
            }

parseGraphite :: [BS.ByteString] -> (String, Key, Time, Double)
parseGraphite = fromJust . decode format . BS.concat
  where
    format = do
        p <- PC.takeTill (== '.') <* PC.char '.'
        k <- PC.takeTill (== ' ') <* PC.char ' '
        v <- PC.double <* PC.char ' '
        t <- PC.decimal
        return (BS.unpack p, Key k, Time t, v)

data Broadcast = Broadcast BS.ByteString [BS.ByteString]
    deriving (Show)

instance Arbitrary Broadcast where
    arbitrary = do
        s  <- arbitrary
        bs <- conduitResult [Receive s] broadcast
        return $ Broadcast s bs