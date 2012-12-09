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
    [ testGroup "metric"
        [ testGroup "encode, then decode"
            [ testProperty "key is equiv" prop_encode_decode_key_equiv
            , testProperty "metric is equiv" prop_encode_decode_metric_equiv
            ]
        ]
    ]

prop_encode_decode_key_equiv :: Encode -> Bool
prop_encode_decode_key_equiv e =
    inputEKey e == outputEKey e

prop_encode_decode_metric_equiv :: Encode -> Bool
prop_encode_decode_metric_equiv e =
    inputEMetric e `kindaCloseM` outputEMetric e

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

data Encode = Encode
    { inputEKey     :: Key
    , inputEMetric  :: Metric
    , outputEStr    :: BS.ByteString
    , outputEKey    :: Key
    , outputEMetric :: Metric
    } deriving (Show)

instance Arbitrary Encode where
    arbitrary = do
        ik <- arbitrary
        im <- arbitrary
        let s        = toByteString $ build (ik, im)
            (ok, om) = fromMaybe ("failed", Counter 0) $ decode lineParser s
        return Encode
            { inputEKey     = ik
            , inputEMetric  = im
            , outputEStr    = s
            , outputEKey    = ok
            , outputEMetric = om
            }
