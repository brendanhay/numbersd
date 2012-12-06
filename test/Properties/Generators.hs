{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Properties.Generators
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Properties.Generators where

import Control.Applicative ((<$>))
import Data.Vector         (fromList)
import Numbers.Types
import Test.QuickCheck

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set              as S

newtype SafeStr = SafeStr String

instance Arbitrary SafeStr where
    arbitrary = SafeStr <$> suchThat arbitrary f
      where
        f s | null s       = False
            | '.' `elem` s = False
            | otherwise    = True

instance Arbitrary Key where
    arbitrary = Key . BS.pack <$> listOf1 (elements ['A'..'z'])

instance Arbitrary Metric where
    arbitrary = oneof
        [ Counter <$> arbitrary
        , Timer . fromList <$> arbitrary
        , Gauge   <$> arbitrary
        , Set     <$> arbitrary
        ]

instance Arbitrary Time where
    arbitrary = do
        NonNegative n <- arbitrary
        return $ Time n

instance Arbitrary Point where
    arbitrary = do
        k             <- arbitrary
        NonNegative v <- arbitrary
        return $ P k v

instance (Ord k, Arbitrary k) => Arbitrary (S.Set k) where
    arbitrary = S.fromList <$> arbitrary

prettyClose :: (Num a, Fractional a, Ord a) => a -> a -> Bool
prettyClose = thisClose 0.0001

kindaClose :: (Num a, Fractional a, Ord a) => a -> a -> Bool
kindaClose = thisClose 0.1

thisClose :: (Num a, Fractional a, Ord a) => a -> a -> a -> Bool
thisClose diff a b
    | a > (b - diff) && (a < b + diff) = True
    | otherwise                        = False
