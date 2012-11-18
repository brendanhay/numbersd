-- |
-- Module      : Properties.GraphiteSink
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Properties.GraphiteSink (graphiteProperties) where

import Control.Applicative        hiding (empty)
import Data.Maybe
import Numbers.Sink.Graphite
import Numbers.Types
import Properties.Generators             ()
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.Attoparsec.Char8 as PC

graphiteProperties :: Test
graphiteProperties = testGroup "graphite sink"
    [ testProperty "formats metrics per graphite spec" prop_format
    ]

prop_format :: Key -> Metric -> Time -> Int -> Bool
prop_format key val ts n =
    (key, read $ flatten val, ts) ==
        fromJust (decode format $ encode (key, val, ts, n))
  where
    format = do
        k <- PC.takeTill (== ' ') <* PC.char ' '
        v <- PC.double <* PC.char ' '
        t <- PC.decimal
        return $ (Key k, v, Time t)
