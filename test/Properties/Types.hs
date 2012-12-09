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
import Control.Applicative                  hiding (empty)
import Data.Attoparsec
import Data.Foldable                               (Foldable, toList)
import Data.Maybe
import Numbers.Types
import Properties.Generators
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.Attoparsec.Char8 as PC
import qualified Data.ByteString.Char8 as BS

typeProperties :: Test
typeProperties = testGroup "types"
    [ testGroup "uri"
        [ testProperty "encode, then decode is equiv" prop_encode_decode_uri_equiv
        ]
    , testGroup "metric"
        [ testGroup "encode, then decode"
            [ testProperty "key is equiv" prop_encode_decode_key_equiv
            , testProperty "metric is equiv" prop_encode_decode_metric_equiv
            ]
        ]
    ]

prop_encode_decode_uri_equiv :: EncodeUri -> Bool
prop_encode_decode_uri_equiv u =
    inputUUri u == outputUUri u

prop_encode_decode_key_equiv :: EncodeMetric -> Bool
prop_encode_decode_key_equiv e =
    inputMKey e == outputMKey e

prop_encode_decode_metric_equiv :: EncodeMetric -> Bool
prop_encode_decode_metric_equiv e =
    inputMMetric e `kindaCloseM` outputMMetric e

data EncodeUri = EncodeUri
    { inputUUri     :: Uri
    , inputUEncoded :: BS.ByteString
    , outputUUri    :: Uri
    } deriving (Show)

instance Arbitrary EncodeUri where
    arbitrary = do
        SafeStr ih  <- arbitrary
        Positive ip <- arbitrary
        iu          <- elements
                           [ File $ BS.pack ih
                           , Tcp (BS.pack ih) ip
                           , Udp (BS.pack ih) ip
                           ]
        let r  = toByteString $ build iu
            ou = fromMaybe (File "failed") $ decode uriParser r
        return EncodeUri
            { inputUUri     = iu
            , inputUEncoded = r
            , outputUUri    = ou
            }

-- | The very pinnacle of scientific engineering
kindaCloseM :: Metric -> Metric -> Bool
kindaCloseM a b = case (a, b) of
    (Counter x, Counter y) -> kindaClose x y
    (Gauge x,   Gauge y)   -> kindaClose x y
    (Timer xs,  Timer ys)  -> f xs ys
    (Set xs,    Set ys)    -> f xs ys
    _                      -> False
  where
    f x y | length i /= length n = error "Not equal lengths"
          | otherwise = and . map (uncurry kindaClose) $ zip i n
      where
        i = toList x
        n = toList y

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
            (ok, om) = fromMaybe ("failed", Counter 0) $ decode lineParser s
        return EncodeMetric
            { inputMKey     = ik
            , inputMMetric  = im
            , outputMStr    = s
            , outputMKey    = ok
            , outputMMetric = om
            }
