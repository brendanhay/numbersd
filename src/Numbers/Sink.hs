{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Numbers.Sink
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Sink (
    -- * Event Constructors
      Event(..)

    -- * Opaque
    , Sink
    , emit
    , runSink

    -- * Lenses
    , receive
    , invalid
    , parse
    , flush

    -- * Sinks
    , graphiteSink
    , broadcastSink
    , downstreamSink
    ) where

import Control.Applicative    hiding (empty)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Data.Lens.Common
import Data.Lens.Template
import Data.Time.Clock.POSIX
import Numbers.Log
import Numbers.Socket
import Numbers.Types

import qualified Data.ByteString.Char8 as BS

data Event = Receive BS.ByteString
           | Invalid BS.ByteString
           | Parse Key Metric
           | Flush Key Metric POSIXTime Int

data Sink = Sink
    { _receive :: BS.ByteString -> IO ()
    , _invalid :: BS.ByteString -> IO ()
    , _parse   :: Key -> Metric -> IO ()
    , _flush   :: Key -> Metric -> POSIXTime -> Int -> IO ()
    , _events  :: TQueue Event
    }

$(makeLens ''Sink)

emit :: [Sink] -> Event -> IO ()
emit sinks evt = forM_ sinks (\s -> atomically $ writeTQueue (_events s) evt)

broadcastSink :: Addr -> IO Sink
broadcastSink addr = do
    r <- openSocketR addr Datagram
    infoL $ "Broadcast connected to " +++ addr
    runSink $ receive ^= \s -> do
        infoL $ "Broadcast: " +++ s +++ " to " +++ addr
        sendR r s

graphiteSink :: String -> Addr -> IO Sink
graphiteSink _prefix addr = do
    infoL $ "Graphite connected to " +++ addr
    runSink $ flush ^= \k v ts n ->
        infoL $ "Graphite: " +++ k ++& v ++& ts

downstreamSink :: Addr -> IO Sink
downstreamSink addr = do
    infoL $ "Upstream connected to " +++ addr
    runSink $ flush ^= \k v ts n ->
        infoL $ "Upstream: " +++ k ++& v ++& ts

runSink :: (Sink -> Sink) -> IO Sink
runSink f = do
    s@Sink{..} <- liftIO $ f <$> newSink
    liftIO . void . forkIO . forever $ do
        e <- liftIO . atomically $ readTQueue _events
        case e of
            (Receive bs)     -> _receive bs
            (Invalid bs)     -> _invalid bs
            (Parse k v)      -> _parse k v
            (Flush k v ts n) -> _flush k v ts n
    return s

newSink :: IO Sink
newSink = Sink
    (\_ -> return ())
    (\_ -> return ())
    (\_ _ -> return ())
    (\_ _ _ _ -> return ())
    <$> atomically newTQueue
