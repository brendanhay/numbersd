{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Numbers.Sink.Overview
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Sink.Overview (
      overviewSink
    ) where

import Blaze.ByteString.Builder hiding (flush)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Data.Lens.Common                ((^=))
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types              (status200, status404)
import Network.HTTP.Types.Status       (Status)
import Numbers.Log
import Numbers.Types
import Numbers.Sink.Internal
import Numbers.Whisper

import qualified Data.ByteString.Char8 as BS

overviewSink :: Maybe Int -> Maybe (IO Sink)
overviewSink = fmap $ \p -> do
    tvar <- atomically . newTVar $ newWhisper 60 10
    void . forkIO $ run p (liftIO . serve tvar)
    infoL $ BS.pack "Overview available at http://0.0.0.0:" +++ p
    runSink $ flush ^= \(k, v, ts, _) ->
        atomically . modifyTVar' tvar $ addMetric k v ts

    -- void . forkIO . forever $ do
    --      threadDelay $ 1000000 * 5
    --      t <- currentTime
    --      m <- readTVarIO tvar
    --      infoL $ BS.pack "Expiring counters " +++ expired t (_counters m)

-- expired :: Time -> Map -> [Key]
-- expired t (Map m) = M.foldWithKey f [] m
--    where
--      f k (v, ts, n) ks = if (t - ts) > 60 then (k:ks) else ks

addMetric :: Key -> Metric -> Time -> Whisper Key -> Whisper Key
addMetric key val ts = addPoint key (Point ts v)
  where
    v = case val of
        (Counter d) -> d
        (Timer ds)  -> sum ds / (fromIntegral $ length ds)
        (Gauge d)   -> d
        (Set _)     -> 1

serve :: TVar (Whisper Key) -> Request -> IO Response
serve tvar req = case rawPathInfo req of
    "/numbersd.json"    -> success json tvar
    "/numbersd.whisper" -> success whisper tvar
    _                   -> return unknown
  where
    json w    = currentTime >>= return . flip jsonSeries w
    whisper w = currentTime >>= return . build . flip textSeries w

success :: (Whisper Key -> IO Builder) -> TVar (Whisper Key) -> IO Response
success f tvar = readTVarIO tvar >>= f >>= return . response status200

unknown :: Response
unknown = response status404 $ copyByteString "{\"error\": \"Not Found\"}"

response :: Status -> Builder -> Response
response code = ResponseBuilder code [("Content-Type", "text/plain")]
