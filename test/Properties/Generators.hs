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
import Numbers.Types
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set              as S

-- TODO: Relax the restriction
instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> listOf1 (elements ['A'..'z'])

instance Arbitrary Key where
    arbitrary = Key <$> arbitrary

instance Arbitrary Metric where
    arbitrary = oneof
        [ Counter <$> arbitrary
        , Timer   <$> arbitrary
        , Gauge   <$> arbitrary
        , Set     <$> arbitrary
        ]

instance Arbitrary Time where
    arbitrary = do
        NonNegative n <- arbitrary
        return $ Time n

instance (Ord k, Arbitrary k) => Arbitrary (S.Set k) where
    arbitrary = S.fromList <$> arbitrary

