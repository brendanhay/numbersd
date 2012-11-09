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

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception      (finally)
import Control.Monad
import Control.Monad.IO.Class
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
        catMaybes [logSink _logEvents, overviewSink _overviewPort] -- _prefix
            ++ map (graphiteSink _prefix) _graphites
            ++ map broadcastSink _broadcasts
            ++ map downstreamSink _downstreams

    infoL "Sinks started..."

    tids  <- newMVar []
    store <- newStore _interval sinks
    input <- atomically newTQueue

    fork tids . runStore store . forever $ do
        bstr <- liftIO . atomically $ readTQueue input
        storeMetric bstr

    mapM_ (fork tids . listener input) _listeners

    infoL "Listeners started..."

    wait tids

listener :: TQueue BS.ByteString -> Uri -> IO ()
listener queue uri | tcp uri   = sock spawn
                   | otherwise = sock push
  where
    sock f  = listen uri >>= forever . f
    spawn s = (forkIO . forever . push) `liftM` accept s
    push s  = (atomically . writeTQueue queue) `liftM` recv s

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
