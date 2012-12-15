{-# LANGUAGE FlexibleContexts, RankNTypes, ImpredicativeTypes #-}

-- |
-- Module      : Numbers.Conduit
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Conduit (
    -- * Exported Types
      Event(..)
    , EventConduit

    -- * Opaque
    , EventSink
    , newSink
    , runSink
    , pushEvents

    -- * Conduits
    , graphite
    , broadcast
    , downstream

    -- * Sources
    , sourceUri

    -- * Sinks
    , sinkUri
    , sinkQueue
    , sinkLog

    -- * Re-exports
    , awaitForever
    , yield
    , (=$)
    , S.withSocketsDo
    ) where

import Blaze.ByteString.Builder           (toByteString)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control        (control)
import Data.Conduit                hiding (Flush)
import Data.Conduit.Binary
import Data.String
import Numbers.Log
import Numbers.Types
import System.IO

import qualified Data.ByteString.Char8     as BS
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as SS
import qualified Data.Conduit.List         as CL
import qualified Data.Conduit.Network      as T
import qualified Data.Conduit.Network.UDP  as U

data Event = Receive BS.ByteString
           | Invalid BS.ByteString
           | Parse Key Metric
           | Flush Key Metric Time
           | Aggregate Point Time
             deriving (Show)

type EventConduit m a = Conduit Event m a

data EventSink = EventSink
    { _queue :: TBQueue Event
    , _async :: Async ()
    }

newSink :: EventConduit IO BS.ByteString -> Uri -> IO EventSink
newSink con uri = runSink $ con =$ transPipe runResourceT (sinkUri uri)

runSink :: Sink Event IO () -> IO EventSink
runSink sink = do
    q <- atomically $ newTBQueue 1024
    a <- async $ sourceQueue q $$ sink
    link a
    return $ EventSink q a

pushEvents :: [EventSink] -> [Event] -> IO ()
pushEvents hs es = sequence_ [f h e | h <- hs, e <- es]
  where
    f h e = atomically $ writeTBQueue (_queue h) e

graphite :: Monad m => String -> EventConduit m BS.ByteString
graphite str = awaitForever $ \e -> case e of
    Aggregate p ts -> yield . toByteString $ pref &&> "." &&& p &&> " " &&& ts &&> "\n"
    _              -> return ()
  where
    pref = BS.pack str

broadcast :: Monad m => EventConduit m BS.ByteString
broadcast = awaitForever $ \e -> case e of
    Receive bs -> yield bs
    _          -> return ()

downstream :: Monad m => EventConduit m BS.ByteString
downstream = awaitForever $ \e -> case e of
    Flush k m _ -> yield . toByteString $ build (k, m)
    _           -> return ()

sourceUri :: Uri -> TBQueue BS.ByteString -> IO ()
sourceUri (File f)  q = runResourceT $
    either sourceFile sourceIOHandle (uriHandle f) $$ sinkQueue q
sourceUri (Tcp h p) q = runResourceT $
    T.runTCPServer (T.serverSettings p $ host h) app
  where
    app d = T.appSource d $$ sinkQueue q
sourceUri (Udp h p) q = control $ \run ->
    bracket open S.sClose (run . forever . sink)
  where
    open   = U.bindPort p $ host h
    sink s = U.sourceSocket s 2048 $$ CL.map U.msgData =$ sinkQueue q

sinkUri :: MonadResource m => Uri -> Sink BS.ByteString m ()
sinkUri (File f) = either sinkFile sinkIOHandle (uriHandle f)
sinkUri uri      = bracketP open S.sClose push
  where
    open = fst `liftM` T.getSocket (_host uri) (_port uri)
    push s = awaitForever $ liftIO . SS.sendAll s

sinkLog :: [String] -> Maybe (IO EventSink)
sinkLog [] = Nothing
sinkLog es = Just $ runSink f
  where
    f = awaitForever $ \e -> g $ case e of
         Receive bs     -> ("receive",   "Receive: "   <&& bs)
         Invalid bs     -> ("invalid",   "Invalid: "   <&& bs)
         Parse k v      -> ("parse"  ,   "Parse: "     <&& (k, v))
         Flush k v ts   -> ("flush"  ,   "Flush: "     <&& (k, v) &&> " " &&& ts)
         Aggregate p ts -> ("aggregate", "Aggregate: " <&& p &&> " " &&& ts)
    g (k, v) = when (k `elem` es) (liftIO $ infoL v)

host :: BS.ByteString -> U.HostPreference
host = fromString . BS.unpack

uriHandle :: BS.ByteString -> Either FilePath (IO Handle)
uriHandle bs = f `fmap` case bs of
    "stdin"  -> Right stdin
    "stderr" -> Right stderr
    "stdout" -> Right stdout
    path     -> Left $ BS.unpack path
  where
    f hd = hSetBuffering hd LineBuffering >> return hd

sourceQueue :: MonadIO m => TBQueue a -> Source m a
sourceQueue q = forever $ liftIO (atomically $ readTBQueue q) >>= yield

sinkQueue :: MonadIO m => TBQueue a -> Sink a m ()
sinkQueue q = awaitForever $ liftIO . atomically . writeTBQueue q
