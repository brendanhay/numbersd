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
import Numbers.Config
import Numbers.Log
import Numbers.Socket
import Numbers.Sink
import Numbers.Numbers

main :: IO ()
main = do
    initLogger

    Options{..} <- parseOptions

    sinks <- sequence $ [logSink logEvents logPath, statusSink status]
        ++ map (graphiteSink graphitePrefix) graphite
        ++ map broadcastSink broadcast
        ++ map downstreamSink downstream

    infoL "Sinks started..."

    (sock, addr) <- openSocket listener Datagram
    bindSocket sock addr

-- start management server here
-- use tvar around sinks inside the Numbers transformer to
-- update? change to a statet?

-- create the tvar here and pass to both runManagement
-- and runServer?

    infoL "Listening..."

    runNumbers interval sinks . forever $ do
        b <- liftIO $ recv sock 1024
        storeMetric b
