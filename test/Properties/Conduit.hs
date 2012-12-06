{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

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

conduitProperties :: Test
conduitProperties = testGroup "sinks"
    [ testGroup "graphite"
        [ testGroup "flush event"
            [ testProperty "encodes prefix" prop_graphite_encodes_prefix
            , testProperty "encodes key" prop_graphite_encodes_key
            , testProperty "encodes value" prop_graphite_encodes_value
            ]
        , testProperty "ignores non flush events" prop_graphite_ignores_non_flush_events
        ]
    , testGroup "broadcast"
        [ testProperty "doesn't modify received packets" prop_broadcast_doesnt_modify_received_packets
        , testProperty "ignores non receive events" prop_broadcast_ignores_non_receive_events
        ]
    , testGroup "downstream"
        [ testProperty "ignores non parse and flush events" prop_downstream_ignores_non_parse_flush_events
        ]
    ]

prop_graphite_encodes_prefix :: Graphite -> Bool
prop_graphite_encodes_prefix evt =
    inputPrefix evt == outputPrefix evt

prop_graphite_encodes_key :: Graphite -> Bool
prop_graphite_encodes_key evt =
    inputKey evt == outputKey evt

prop_graphite_encodes_value :: Graphite -> Bool
prop_graphite_encodes_value evt =
    kindaClose (inputValue evt) (outputValue evt)

prop_graphite_ignores_non_flush_events :: Property
prop_graphite_ignores_non_flush_events =
    forAll (conduitEvent (graphite "") p) null
  where
    p (Flush _ _) = False
    p _           = True

prop_broadcast_doesnt_modify_received_packets :: Broadcast -> Bool
prop_broadcast_doesnt_modify_received_packets (Broadcast s bs) =
    [s] == bs

prop_broadcast_ignores_non_receive_events :: Property
prop_broadcast_ignores_non_receive_events =
    forAll (conduitEvent broadcast p) null
  where
    p (Receive _) = False
    p _           = True

prop_downstream_ignores_non_parse_flush_events :: Property
prop_downstream_ignores_non_parse_flush_events =
    forAll (conduitEvent downstream p) null
  where
    p (Parse _ _) = False
    p (Flush _ _) = False
    p _           = True

conduitEvent :: EventConduit Gen BS.ByteString
             -> (Event -> Bool)
             -> Gen [BS.ByteString]
conduitEvent con p = do
    e <- suchThat arbitrary p
    conduitResult e con

data Graphite = Graphite
    { inputPrefix  :: String
    , inputKey     :: Key
    , inputTime    :: Time
    , inputValue   :: Double
    , outputPrefix :: String
    , outputKey    :: Key
    , outputTime   :: Time
    , outputValue  :: Double
    } deriving (Show)

instance Arbitrary Graphite where
    arbitrary = do
        SafeStr ip  <- arbitrary
        it          <- arbitrary
        p@(P ik iv) <- arbitrary
        bs          <- conduitResult (Flush it p) (graphite ip)
        let (op, ok, ot, ov) = parseGraphite bs
        return $ Graphite
            { inputPrefix  = ip
            , inputKey     = ik
            , inputTime    = it
            , inputValue   = iv
            , outputPrefix = op
            , outputKey    = ok
            , outputTime   = ot
            , outputValue  = ov
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
        bs <- conduitResult (Receive s) broadcast
        return $ Broadcast s bs