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
    -- * Event Type
      Event(..)
    , EventName

    -- * Opaque
    , Sink
    , emit

    -- * Sinks
    , logSink
    , graphiteSink
    , broadcastSink
    , downstreamSink
    , statusSink
    ) where

import Control.Applicative    hiding (empty)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Data.Setters
import Data.String
import Data.Time.Clock.POSIX
import System.IO
import Numbers.Log
import Numbers.Metric
import Numbers.Socket

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types       (status200)
import Blaze.ByteString.Builder (copyByteString)
import Data.Monoid

import qualified Data.ByteString.Char8 as BS

data Event = Receive BS.ByteString
           | Invalid BS.ByteString
           | Parse Key Metric
           | Flush Key Metric POSIXTime Int

newtype EventName = EventName String deriving (Eq)

instance Read EventName where
    readsPrec _ a = do
        (h, b) <- lex a
        return (EventName h, b)

instance Show EventName where
    show (EventName s) = s

instance IsString EventName where
    fromString = EventName

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

logSink :: [EventName] -> String -> IO Sink
logSink evts path = do
    l <- newLogger "Log" path
    runSink $ \s -> foldl f s (logEvents l)
  where
    f s (k, g) = if k `elem` evts
                  then g s
                  else s

logEvents :: (String -> IO ()) -> [(EventName, Sink -> Sink)]
logEvents f =
    [ ("receive", setReceive $ \v ->   f $ "Receive: " ++ BS.unpack v)
    , ("invalid", setInvalid $ \v ->   f $ "Invalid: " ++ BS.unpack v)
    , ("parse", setParse $ \k v ->     f $ "Parse: " ++ show k ++ " " ++ show v)
    , ("flush", setFlush $ \k v _ _ -> f $ "Flush: " ++ show k ++ " " ++ show v)
    ]

broadcastSink :: Addr -> IO Sink
broadcastSink addr = do
    r <- openSocketR addr Datagram
    infoL $ "Broadcast connected to " ++ show addr
    runSink . setReceive $ \s -> do
        infoL $ "Broadcast: " ++ BS.unpack s ++ " to " ++ show addr
        sendR r s

graphiteSink :: String -> Addr -> IO Sink
graphiteSink prefix addr = do
    infoL $ "Graphite connected to " ++ show addr
    runSink . setFlush $ \k v ts n -> do
        infoL $ "Graphite: " ++ show k ++ " " ++ show v ++ " " ++ show ts

downstreamSink :: Addr -> IO Sink
downstreamSink addr = do
    infoL $ "Upstream connected to " ++ show addr
    runSink . setFlush $ \k v ts n -> do
        infoL $ "Upstream: " ++ show k ++ " " ++ show v ++ " " ++ show ts

statusSink :: Addr -> IO Sink
statusSink (Addr _ port) = do
    forkIO $ run port app

    runSink . setFlush $ \k _ _ _ -> do
        putStrLn $ "Status: " ++ show k ++ " on " ++ show port

app req = return builderNoLen

builderNoLen = ResponseBuilder
    status200
    [ ("Content-Type", "text/plain")
    ]
    $ copyByteString "PONG"

runSink :: (Sink -> Sink) -> IO Sink
runSink f = do
    s@Sink{..} <- liftIO $ f <$> newSink
    liftIO . void . forkIO . forever $ do
        e <- liftIO . atomically $ readTQueue events
        case e of
            (Receive bs)     -> receive bs
            (Invalid bs)     -> invalid bs
            (Parse k v)      -> parse k v
            (Flush k v ts n) -> flush k v ts n
    return s

newSink :: IO Sink
newSink = Sink
    (\_ -> return ())
    (\_ -> return ())
    (\_ _ -> return ())
    (\_ _ _ _ -> return ())
    <$> atomically newTQueue
