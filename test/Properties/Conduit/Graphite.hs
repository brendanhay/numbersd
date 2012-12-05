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

import Blaze.ByteString.Builder                    (toByteString)
import Control.Applicative                  hiding (empty)
import Data.Attoparsec.Combinator                  (many1)
import Data.List
import Data.Maybe
import Numbers.Conduit
import Numbers.Types
import Properties.Generators                       ()
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.Attoparsec.Char8 as PC
import qualified Data.ByteString.Char8 as BS

graphiteProperties :: Test
graphiteProperties = testGroup "graphite sink"
    [-- testProperty "encodes flush event" prop_encodes_flush_event
    ]

-- prop_encodes_flush_event :: EventEmit -> Bool
-- prop_encodes_flush_event e =
--     stripPrefix (inputPrefix e) (outputStr e) == Just (inputPrefix e)

instance Arbitrary Point where
    arbitrary = do
        k <- arbitrary
        NonNegative (v :: Int) <- arbitrary
        return $ P k $ fromIntegral v

data EventEmit = EventEmit
    { inputPoint  :: Point
    , inputPrefix :: String
    , outputStr   :: String
    } deriving (Show)

instance Arbitrary EventEmit where
    arbitrary = do
        p <- arbitrary
        NonEmpty s <- arbitrary
        return $ EventEmit
            { inputPoint  = p
            , inputPrefix = s
            , outputStr   = BS.unpack . toByteString $ build p
            }
