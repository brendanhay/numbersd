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

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe             (catMaybes)
import Numbers.Config
import Numbers.Log
import Numbers.Sink
import Numbers.Socket
import Numbers.Store
import Numbers.Types

import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = withSocketsDo $ do
    Config{..} <- parseConfig

    sinks <- sequence $
        catMaybes [logSink _logEvents, overviewSink _overviewPort]
            ++ map (graphiteSink _prefix) _graphites
            ++ map broadcastSink _broadcasts
            ++ map downstreamSink _downstreams

    infoL "Sinks started..."

    buf <- atomically newTQueue

    mapM_ (forkIO . listener buf) _listeners

    infoL "Listeners started..."

    store <- newStore _interval sinks

    forever $ do
        bstr <- atomically $ readTQueue buf
        insert store bstr

listener :: TQueue BS.ByteString -> Uri -> IO ()
listener buf uri | tcp uri   = f tcpListener
                 | otherwise = f udpListener
  where
    f g = listen uri >>= forever . g buf

tcpListener :: TQueue BS.ByteString -> Socket -> IO ()
tcpListener buf sock = do
    s <- accept sock
    void . forkIO . forever $ do
        b <- recv s
        atomically $ writeTQueue buf b

udpListener :: TQueue BS.ByteString -> Socket -> IO ()
udpListener buf sock = recv sock >>= atomically . writeTQueue buf
