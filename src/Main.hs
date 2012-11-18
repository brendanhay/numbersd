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
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe               (catMaybes)
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
        catMaybes [ logSink _logEvents
                  , httpSink _resolution _interval _httpPort
                  ]
            ++ map (graphiteSink _prefix) _graphites
            ++ map broadcastSink _broadcasts
            ++ map downstreamSink _downstreams

    infoL "Sinks started..."

    store <- newStore _interval sinks
    buf   <- atomically newTQueue

    infoL "Buffering..."

    tids  <- mapM asyncLink $ reader buf store:map (listener buf) _listeners

    infoL "Listeners started..."

    -- Communication between the main thread and other forkIO'd
    -- threads is much much slower than between two forkIO'd threads
    -- Just waiting in the main thread
    void $ waitAnyCancel tids

reader :: TQueue BS.ByteString -> Store -> IO ()
reader buf store = forever $ do
    bstr <- atomically $ readTQueue buf
    parse bstr store

listener :: TQueue BS.ByteString -> Uri -> IO ()
listener buf uri
    | tcp uri   = f tcpListener
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

asyncLink :: IO a -> IO (Async a)
asyncLink io = do
    a <- async io
    link a
    return a