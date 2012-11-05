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
    , EventName

    -- * Opaque
    , Sink
    , emit

    -- * Sinks
    , consoleSink
    , repeaterSink
    ) where

import Control.Applicative    hiding (empty)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Data.Setters
import Data.String
import Data.Time.Clock.POSIX
import Vodki.Metric
import Vodki.Socket

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

consoleSink :: [EventName] -> IO Sink
consoleSink evts = runSink $ (flip $ foldl f) consoleF
  where
    f s (k, g) = if k `elem` evts then g s else s

consoleF :: [(EventName, Sink -> Sink)]
consoleF =
    [ ("receive", setReceive $ \v -> putStrLn $ "Receive: " ++ BS.unpack v)
    , ("invalid", setInvalid $ \v -> putStrLn $ "Invalid: " ++ BS.unpack v)
    , ("parse", setParse $ \k v -> putStrLn $ "Parse: " ++ show k ++ " " ++ show v)
    , ("flush", setFlush $ \k v _ _ -> putStrLn $ "Flush: " ++ show k ++ " " ++ show v)
    ]

repeaterSink :: Addr -> IO Sink
repeaterSink addr = do
    r <- openSocketR addr Datagram
    putStrLn $ "Repeater connected to " ++ show addr
    runSink . setReceive $ \s -> do
        putStrLn $ "Repeat: " ++ BS.unpack s ++ " to " ++ show addr
        sendR r s

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
