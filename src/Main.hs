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
        catMaybes [logSink _logEvents, overviewSink _overviewPort]
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

listener queue u@(Tcp _ _) = do
    s <- listen u
    forever $ do
        c <- accept s
        void . forkIO . forever $ do
            b <- recv c
            atomically $ writeTQueue queue b
listener queue u@(Udp _ _) = do
    s <- listen u
    forever $ do
        b <- recv s
        atomically $ writeTQueue queue b

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
