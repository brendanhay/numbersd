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

import Control.Applicative        ((<$>))
import Data.Conduit        hiding (Flush)
import Data.Vector                (fromList)
import Numbers.Conduit
import Numbers.Types
import Numeric                    (showFFloat)
import Test.QuickCheck

import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.List     as CL
import qualified Data.Set              as S

instance Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> arbitrary

newtype SafeStr = SafeStr String

instance Arbitrary SafeStr where
    arbitrary = SafeStr <$> (listOf1 $ elements xs)
        where
          xs = ['_', '-'] ++ ['a'..'z'] ++ ['A'..'Z']

instance Arbitrary Key where
    arbitrary = do
        SafeStr s <- arbitrary
        return . Key $ BS.pack s

instance Arbitrary Metric where
    arbitrary = oneof
        [ Counter <$> arbitrary
        , arbitrary >>= \(NonEmpty xs) -> return . Timer $ fromList xs
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

instance Arbitrary (S.Set Double) where
    arbitrary = do
        NonEmpty xs <- arbitrary
        return . S.fromList $ map f xs
      where
        f x = read $ showFFloat (Just 1) (x :: Double) ""

instance Arbitrary Event where
    arbitrary = do
        s <- BS.pack <$> arbitrary
        k <- arbitrary
        m <- arbitrary
        t <- arbitrary
        p <- arbitrary
        elements
            [ Receive s
            , Invalid s
            , Parse k m
            , Flush k m t
            , Aggregate p t
            ]

kindaClose :: (Num a, Fractional a, Ord a) => a -> a -> Bool
kindaClose = thisClose 0.1

prettyClose :: (Num a, Fractional a, Ord a) => a -> a -> Bool
prettyClose = thisClose 0.0001

thisClose :: (Num a, Fractional a, Ord a) => a -> a -> a -> Bool
thisClose diff a b
    | a > (b - diff) && (a < b + diff) = True
    | otherwise                        = False

conduitResult :: Monad m => [Event] -> EventConduit m a -> m [a]
conduitResult es con = CL.sourceList es $= con $$ CL.consume
