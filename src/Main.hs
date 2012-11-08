-- |
-- Module      : Main
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main (
    -- * Entry Point
      main
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe             (catMaybes)
import Numbers.Config
import Numbers.Log
import Numbers.Sink
import Numbers.Sink.Log
import Numbers.Sink.Status
import Numbers.Socket
import Numbers.Store

main :: IO ()
main = withSocketsDo $ do
    Options{..} <- parseOptions

    sinks <- sequence $ catMaybes [logSink _logEvents _logPath, statusSink _status]
        ++ map (graphiteSink _graphitePrefix) _graphite
        ++ map broadcastSink _broadcast
        ++ map downstreamSink _downstream

    infoL "Sinks started..."

    (s, a) <- openSocket _listener Datagram
    bindSocket s a

-- start management server here
-- use tvar around sinks inside the Numbers transformer to
-- update? change to a statet?

-- create the tvar here and pass to both runManagement
-- and runServer?

    infoL "Listening..."

    runStore _interval sinks . forever $ do
        b <- liftIO $ recv s 1024
        storeMetric b
