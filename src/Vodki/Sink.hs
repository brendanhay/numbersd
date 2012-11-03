{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Vodki.Sink
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Vodki.Sink (
    -- * Event Type
      Event(..)

    -- * Opaque
    , Sink
    , emit

    -- * Sinks
    , dumpMessages
    , repeater
    , graphite
    ) where

import Control.Applicative        hiding (empty)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Setters
import Data.Time.Clock.POSIX
import Vodki.Metric

import qualified Data.ByteString.Char8 as BS

data Event = Receive BS.ByteString
           | Invalid BS.ByteString
           | Parse Key Metric
           | Flush Key Metric POSIXTime Int

data Sink = Sink
    { receive :: BS.ByteString -> IO ()
    , invalid :: BS.ByteString -> IO ()
    , parse   :: Key -> Metric -> IO ()
    , flush   :: Key -> Metric -> POSIXTime -> Int -> IO ()
    , events  :: TQueue Event
    }

$(declareSetters ''Sink)

emit :: [Sink] -> Event -> IO ()
emit sinks evt = forM_ sinks (\s -> atomically $ writeTQueue (events s) evt)

dumpMessages :: IO Sink
dumpMessages = runSink . setParse $ \k v ->
    putStrLn $ "Dump: " ++ show k ++ " " ++ show v

repeater :: String -> Int -> IO Sink
repeater host port = runSink . setReceive $ \s ->
    putStrLn $ "Repeat: " ++ BS.unpack s

graphite :: String -> Int -> IO Sink
graphite host port = runSink . setFlush $ \k v ts _ ->
    putStrLn $ "Graphite: " ++ show k ++ " " ++ show v ++ " " ++ show ts

newSink :: IO Sink
newSink = Sink f f (\_ _ -> return ()) (\_ _ _ _ -> return ())
    <$> atomically newTQueue
  where
    f _ = return ()

runSink :: (Sink -> Sink) -> IO Sink
runSink f = do
    s@Sink{..} <- f <$> newSink
    void . forkIO . forever $ do
        e <- atomically $ readTQueue events
        case e of
            (Receive bs)     -> receive bs
            (Invalid bs)     -> invalid bs
            (Parse k v)      -> parse k v
            (Flush k v ts n) -> flush k v ts n
    return s
