{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- |
-- Module      : Properties.Conduit.Graphite
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Properties.Conduit.Graphite (graphiteProperties) where

import Control.Applicative                  hiding (empty)
import Data.Conduit                         hiding (Flush)
import Data.List.Split                             (splitOn)
import Numbers.Conduit
import Numbers.Types
import Properties.Generators                       ()
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.List         as CL

graphiteProperties :: Test
graphiteProperties = testGroup "graphite sink"
    [ testGroup "encodes"
        [ testProperty "prefix" prop_encodes_prefix
        ]
    ]

prop_encodes_prefix :: EventEmit -> Bool
prop_encodes_prefix e =
     outputPrefix e == inputPrefix e

instance Arbitrary Point where
    arbitrary = do
        k <- arbitrary
        NonNegative (v :: Int) <- arbitrary
        return $ P k $ fromIntegral v

data EventEmit = EventEmit
    { inputPoint   :: Event
    , inputPrefix  :: String
    , outputPrefix :: String
    } deriving (Show)

newtype SafeStr = SafeStr String

instance Arbitrary SafeStr where
    arbitrary = SafeStr <$> suchThat arbitrary f
      where
        f s | null s       = False
            | '.' `elem` s = False
            | otherwise    = True

instance Arbitrary EventEmit where
    arbitrary = do
        SafeStr s <- arbitrary
        ts <- arbitrary
        p  <- arbitrary
        l  <- CL.sourceList [Flush ts p] $= graphite s $$ CL.consume
        let r = BS.unpack $ BS.concat l
        return $ EventEmit
            { inputPoint   = Flush ts p
            , inputPrefix  = s
            , outputPrefix = head $ splitOn "." r
            }
