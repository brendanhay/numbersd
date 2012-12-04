{-# LANGUAGE FlexibleContexts #-}

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

module Numbers.Conduit.Internal (
    -- * Exported Types
      Event(..)

    -- * Opaque
    , EventSink
    , runSink
    , pushEvent

    -- * Sources
    , sourceUri
    , sourceQueue

    -- * Sinks
    , sinkUri
    , sinkQueue

    -- * Re-exports
    , awaitForever
    , yield
    , (=$)
    , S.withSocketsDo
    ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (control)
import Data.Conduit
import Data.Conduit.Binary
import Data.String
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
           | Flush Time Point
             deriving (Show)

data EventSink = EventSink
    { _queue :: TBQueue Event
    , _sink  :: Sink Event (ResourceT IO) ()
    }

runSink :: Sink Event (ResourceT IO) () -> IO EventSink
runSink sink = do
    h <- handler
    fork h >>= link
    return h
  where
    handler = do
        q <- atomically (newTBQueue 1024)
        return $ EventSink q sink
    fork (EventSink q s) = async . runResourceT $ sourceQueue q $$ s

pushEvent :: [EventSink] -> Event -> IO ()
pushEvent hs evt = forM_ hs (\h -> atomically $ writeTBQueue (_queue h) evt)

sourceUri :: Uri -> TBQueue BS.ByteString -> IO ()
sourceUri (File f)  q = runResourceT $
    either sourceIOHandle sourceFile (uriHandle f) $$ sinkQueue q
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
sinkUri (File f) = either sinkIOHandle sinkFile (uriHandle f)
sinkUri uri      = bracketP open S.sClose push
  where
    open = fst `liftM` T.getSocket (_host uri) (_port uri)
    push s = awaitForever $ liftIO . SS.sendAll s

host :: BS.ByteString -> U.HostPreference
host = fromString . BS.unpack

uriHandle :: BS.ByteString -> Either (IO Handle) FilePath
uriHandle "stdin"  = Left  $ return stdin
uriHandle "stderr" = Left  $ return stderr
uriHandle "stdout" = Left  $ return stdout
uriHandle f        = Right $ BS.unpack f

sourceQueue :: MonadIO m => TBQueue a -> Source m a
sourceQueue q = forever $ liftIO (atomically $ readTBQueue q) >>= yield

sinkQueue :: MonadIO m => TBQueue a -> Sink a m ()
sinkQueue q = awaitForever $ liftIO . atomically . writeTBQueue q
