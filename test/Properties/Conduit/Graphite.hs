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

module Properties.Conduit.Graphite (
      graphiteProperties
    ) where

import Control.Applicative                  hiding (empty)
import Data.Conduit                         hiding (Flush)
import Data.Maybe
import Numbers.Conduit
import Numbers.Types
import Properties.Generators
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.Attoparsec.Char8 as PC
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.List     as CL

graphiteProperties :: Test
graphiteProperties = testGroup "graphite sink"
    [ testGroup "encodes"
        [ testProperty "prefix" prop_encodes_prefix
        , testProperty "key" prop_encodes_key
        , testProperty "value" prop_encodes_value
        ]
    ]

prop_encodes_prefix :: EventEmit -> Bool
prop_encodes_prefix e =
    inputPrefix e == outputPrefix e

prop_encodes_key :: EventEmit -> Bool
prop_encodes_key e =
    inputKey e == outputKey e

prop_encodes_value :: EventEmit -> Bool
prop_encodes_value e =
    kindaClose (inputValue e) (outputValue e)

instance Arbitrary Point where
    arbitrary = do
        k             <- arbitrary
        NonNegative v <- arbitrary
        return $ P k v

data EventEmit = EventEmit
    { inputPrefix  :: String
    , inputKey     :: Key
    , inputTime    :: Time
    , inputValue   :: Double
    , outputPrefix :: String
    , outputKey    :: Key
    , outputTime   :: Time
    , outputValue  :: Double
    } deriving (Show)

instance Arbitrary EventEmit where
    arbitrary = do
        SafeStr ip  <- arbitrary
        it          <- arbitrary
        p@(P ik iv) <- arbitrary
        bs          <- CL.sourceList [Flush it p] $= graphite ip $$ CL.consume
        let (op, ok, ot, ov) = parse bs
        return $ EventEmit
            { inputPrefix  = ip
            , inputKey     = ik
            , inputTime    = it
            , inputValue   = iv
            , outputPrefix = op
            , outputKey    = ok
            , outputTime   = ot
            , outputValue  = ov
            }

parse :: [BS.ByteString] -> (String, Key, Time, Double)
parse = fromJust . decode format . BS.concat
  where
    format = do
        p <- PC.takeTill (== '.') <* PC.char '.'
        k <- PC.takeTill (== ' ') <* PC.char ' '
        v <- PC.double <* PC.char ' '
        t <- PC.decimal
        return (BS.unpack p, Key k, Time t, v)