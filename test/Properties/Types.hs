{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Properties.Types
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Properties.Types (
      typeProperties
    ) where

import Prelude                              hiding (foldl)
import Blaze.ByteString.Builder                    (toByteString)
import Data.Maybe
import Data.Monoid
import Data.List                                   (union)
import Numbers.Types
import Properties.Generators
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set              as S
import qualified Data.Vector           as V

typeProperties :: Test
typeProperties = testGroup "types"
    [ testGroup "uri"
        [ testGroup "encode, then decode"
              [ testProperty "uri is equiv" prop_encode_decode_uri_equiv
              , testProperty "host is equiv" prop_encode_decode_uri_host_equiv
              , testProperty "port is equiv" prop_encode_decode_uri_port_equiv
              ]
        ]
    , testGroup "key"
        [ testProperty "encode, then decode is equiv" prop_encode_decode_key_equiv
        , testProperty "decode strips unsafe chars" prop_decode_key_strips_unsafe
        , testProperty "mappend/mconcat is dot delimited" prop_mconcat_keys_is_dot_delimited
        ]
    , testGroup "metric"
        [ testGroup "encode, then decode"
            [ testProperty "key is equiv" prop_encode_decode_metric_key_equiv
            , testProperty "metric is close enough" prop_encode_decode_metric_equiv
            ]
        , testProperty "is not zeroed" prop_metric_is_not_zeroed
        , testGroup "aggregate"
            [ testProperty "with nothing, keeps original" prop_aggregate_metric_with_nothing
            , testProperty "with different ctor, keeps rvalue" prop_aggregate_disimilar_metrics_keep_rvalue
            , testProperty "counters are summed" prop_aggregate_counters_are_summed
            , testProperty "gauges keep rvalue" prop_aggregate_gauges_keep_rvalue
            , testProperty "timers are prepended" prop_aggregate_timers_are_prepended
            , testProperty "sets are unioned" prop_aggregate_sets_are_unioned
            ]
        ]
    ]

prop_encode_decode_uri_equiv :: EncodeUri -> Bool
prop_encode_decode_uri_equiv u =
    inputUUri u == outputUUri u

prop_encode_decode_uri_host_equiv :: EncodeUri -> Bool
prop_encode_decode_uri_host_equiv u =
    inputUHost u == outputUHost u

prop_encode_decode_uri_port_equiv :: EncodeUri -> Bool
prop_encode_decode_uri_port_equiv u =
    inputUPort u == outputUPort u

prop_encode_decode_key_equiv :: EncodeKey -> Bool
prop_encode_decode_key_equiv k =
    inputKKey k == outputKKey k

prop_encode_decode_metric_key_equiv :: EncodeMetric -> Bool
prop_encode_decode_metric_key_equiv e =
    inputMKey e == outputMKey e

prop_decode_key_strips_unsafe :: UnsafeStr -> Bool
prop_decode_key_strips_unsafe (UnsafeStr s) =
    BS.all (\c -> c `elem` valid) k
  where
    (Key k) = fromMaybe "failed!" . decode keyParser . BS.pack $ map f s
    f ':' = '_'
    f c   = c
    valid = ['a'..'z'] ++ ['0'..'9'] ++ ['A'..'Z'] ++ ['_', '-', '.']

prop_mconcat_keys_is_dot_delimited :: [Key] -> Bool
prop_mconcat_keys_is_dot_delimited ks =
    length ks == BS.count '.' s
  where
    (Key s) = mconcat ks

prop_encode_decode_metric_equiv :: EncodeMetric -> Bool
prop_encode_decode_metric_equiv e =
    inputMMetric e `kindaCloseM` outputMMetric e

prop_metric_is_not_zeroed :: Property
prop_metric_is_not_zeroed =
    forAll f (not . zero)
  where
    f = do
        Positive v  <- arbitrary
        NonEmpty xs <- arbitrary
        elements [ Counter v
                 , Gauge   v
                 , Timer   $ V.fromList xs
                 , Set     $ S.fromList xs
                 ]

prop_aggregate_metric_with_nothing :: Metric -> Bool
prop_aggregate_metric_with_nothing m =
    m `aggregate` Nothing == m

prop_aggregate_disimilar_metrics_keep_rvalue :: Property
prop_aggregate_disimilar_metrics_keep_rvalue =
    forAll f $ \(a, b) -> a `aggregate` (Just b) == b
  where
    f = suchThat arbitrary (not . uncurry similarM)

prop_aggregate_counters_are_summed :: Double -> Double -> Bool
prop_aggregate_counters_are_summed x y =
    aggregate (Counter x) (Just $ Counter y) == Counter (x + y)

prop_aggregate_gauges_keep_rvalue :: Double -> Double -> Bool
prop_aggregate_gauges_keep_rvalue x y =
    aggregate (Gauge x) (Just $ Gauge y) == Gauge y

prop_aggregate_timers_are_prepended :: [Double] -> [Double] -> Bool
prop_aggregate_timers_are_prepended xs ys =
    aggregate (f xs) (Just $ f ys) == f (ys ++ xs)
  where
    f = Timer . V.fromList

prop_aggregate_sets_are_unioned :: [Double] -> [Double] -> Bool
prop_aggregate_sets_are_unioned xs ys =
    aggregate (f xs) (Just $ f ys) == f (xs `union` ys)
  where
    f = Set . S.fromList

similarM :: Metric -> Metric -> Bool
similarM a b = case (a, b) of
    (Counter{}, Counter{}) -> True
    (Gauge{},   Gauge{})   -> True
    (Timer{},   Timer{})   -> True
    (Set{},     Set{})     -> True
    _                      -> False

data EncodeUri = EncodeUri
    { inputUUri     :: Uri
    , inputUHost    :: BS.ByteString
    , inputUPort    :: Int
    , inputUEncoded :: BS.ByteString
    , outputUUri    :: Uri
    , outputUHost   :: BS.ByteString
    , outputUPort   :: Int
    } deriving (Show)

instance Arbitrary EncodeUri where
    arbitrary = do
        SafeStr ih  <- arbitrary
        Positive ip <- arbitrary
        iu          <- elements [ File $ BS.pack ih
                                , Tcp (BS.pack ih) ip
                                , Udp (BS.pack ih) ip
                                ]
        let r  = toByteString $ build iu
            ou = fromMaybe (File "failed!") $ decode uriParser r
        return EncodeUri
            { inputUUri     = iu
            , inputUHost    = BS.pack ih
            , inputUPort    = ip
            , inputUEncoded = r
            , outputUUri    = ou
            , outputUHost   = host ou
            , outputUPort   = port ou ip
            }
      where
        host (File f) = f
        host u        = _host u
        port File{} p = p
        port u _      = _port u

data EncodeKey = EncodeKey
    { inputKKey     :: Key
    , inputKEncoded :: BS.ByteString
    , outputKKey    :: Key
    } deriving (Show)

instance Arbitrary EncodeKey where
    arbitrary = do
        ik <- arbitrary
        let r  = toByteString $ build ik
            ok = fromMaybe "failed!" $ decode keyParser r
        return EncodeKey
            { inputKKey     = ik
            , inputKEncoded = r
            , outputKKey    = ok
            }

data EncodeMetric = EncodeMetric
    { inputMKey     :: Key
    , inputMMetric  :: Metric
    , outputMStr    :: BS.ByteString
    , outputMKey    :: Key
    , outputMMetric :: Metric
    } deriving (Show)

instance Arbitrary EncodeMetric where
    arbitrary = do
        ik <- arbitrary
        im <- arbitrary
        let s        = toByteString $ build (ik, im)
            (ok, om) = fromMaybe ("failed!", Counter 0) $ decode lineParser s
        return EncodeMetric
            { inputMKey     = ik
            , inputMMetric  = im
            , outputMStr    = s
            , outputMKey    = ok
            , outputMMetric = om
            }
