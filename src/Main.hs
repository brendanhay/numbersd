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
import Control.Exception      (finally)
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
        catMaybes [ logSink _logEvents
                  , httpSink _resolution _interval _httpPort
                  ]
            ++ map (graphiteSink _prefix) _graphites
            ++ map broadcastSink _broadcasts
            ++ map downstreamSink _downstreams

    infoL "Sinks started..."

    tids  <- newMVar []
    store <- newStore _interval sinks
    buf   <- atomically newTQueue

    infoL "Buffering..."

    mapM_ (fork tids . listener buf) _listeners

    infoL "Listeners started..."

    -- Communication between the main thread and other forkIO'd
    -- threads is much much slower than between two forkIO'd threads
    fork tids . forever $ do
        bstr <- atomically $ readTQueue buf
        parse bstr store

    -- Just waiting in the main thread
    wait tids

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

fork :: MVar [MVar ()] -> IO () -> IO ThreadId
fork tids io = do
    t  <- newEmptyMVar
    ts <- takeMVar tids
    putMVar tids $ t:ts
    forkIO (io `finally` putMVar t ())

wait :: MVar [MVar a] -> IO ()
wait tids = do
    ts <- takeMVar tids
    case ts of
        []   -> return ()
        m:ms -> do
            putMVar tids ms
            _ <- takeMVar m
            wait tids

-- -- |
-- -- Module      : Main
-- -- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- -- License     : This Source Code Form is subject to the terms of
-- --               the Mozilla Public License, v. 2.0.
-- --               A copy of the MPL can be found in the LICENSE file or
-- --               you can obtain it at http://mozilla.org/MPL/2.0/.
-- -- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- -- Stability   : experimental
-- -- Portability : non-portable (GHC extensions)
-- --

-- module Main where

-- import Control.Concurrent.STM
-- import Control.Monad
-- import Data.Maybe             (catMaybes)
-- import Numbers.Config
-- import Numbers.Log
-- import Numbers.Sink
-- import Numbers.Socket
-- import Numbers.Store
-- import Numbers.ThreadManager
-- import Numbers.Types

-- import qualified Data.ByteString.Char8 as BS

-- main :: IO ()
-- main = withSocketsDo $ do
--     Config{..} <- parseConfig

--     sinks <- sequence $
--         catMaybes [ logSink _logEvents
--                   , httpSink _resolution _interval _httpPort
--                   ]
--             ++ map (graphiteSink _prefix) _graphites
--             ++ map broadcastSink _broadcasts
--             ++ map downstreamSink _downstreams

--     infoL "Sinks started..."

--     tm    <- newManager
--     store <- newStore _interval sinks
--     buf   <- atomically newTQueue

--     infoL "Buffering..."

--     mapM_ (fork tm . listener tm buf) _listeners

--     infoL "Listeners started..."

--     -- Communication between the main thread and other forkIO'd
--     -- threads is much much slower than between two forkIO'd threads
--     fork tm . forever $ do
--         bstr <- atomically $ readTQueue buf
--         parse bstr store

--     -- Just hanging out in the main thread
--     waitAll tm

-- listener :: ThreadManager -> TQueue BS.ByteString -> Uri -> IO ()
-- listener tm buf uri | tcp uri   = f (tcpListener tm)
--                     | otherwise = f udpListener
--   where
--     f g = listen uri >>= forever . g buf

-- tcpListener :: ThreadManager -> TQueue BS.ByteString -> Socket -> IO ()
-- tcpListener tm buf sock = do
--     s <- accept sock
--     void . fork tm . forever $ do
--         b <- recv s
--         atomically $ writeTQueue buf b

-- udpListener :: TQueue BS.ByteString -> Socket -> IO ()
-- udpListener buf sock = recv sock >>= atomically . writeTQueue buf
