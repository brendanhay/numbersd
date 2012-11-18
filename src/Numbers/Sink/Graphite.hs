-- |
-- Module      : Numbers.Sink.Graphite
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Sink.Graphite (
      graphiteSink

    -- * Testing .....
    , encode
    , flatten
    ) where

import Numeric               (showFFloat)
import Data.Lens.Common
import Numbers.Log
import Numbers.Sink.Internal
import Numbers.Socket
import Numbers.Types

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set              as S

graphiteSink :: String -> Uri -> IO Sink
graphiteSink prefix uri = do
    sock <- connect uri
    runSink $ flush ^= flushMetric prefix sock

flushMetric :: String -> Socket -> (Key, Metric, Time, Int) -> IO ()
flushMetric prefix sock evt = do
    let bs = encode evt
    send sock $ bs `BS.append` BS.pack "\n"
    infoL $ "Graphite: " +++ bs

encode :: (Key, Metric, Time, Int) -> BS.ByteString
encode (Key key, val, Time ts, _) =
    BS.append key . BS.pack $ concat
        [ " "
        , flatten val
        , " "
        , show ts
        , "\n"
        ]

flatten :: Metric -> String
flatten val = showFFloat (Just 2) x ""
  where
    x = case val of
            (Counter n) -> n
            (Timer  ns) -> sum ns
            (Gauge   n) -> n
            (Set    ss) -> S.fold (+) 0 ss
