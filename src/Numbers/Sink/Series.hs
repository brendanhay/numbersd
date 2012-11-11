{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Numbers.Sink.Series
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Sink.Series (
      seriesSink
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
import Numbers.Log
import Numbers.Types
import Numbers.Sink.Internal
import Numbers.Whisper

import qualified Data.ByteString.Char8 as BS

seriesSink :: Resolution -> Step -> Maybe Integer -> Maybe (IO Sink)
seriesSink res step = fmap $ \p -> do
    tvar <- atomically . newTVar $ newWhisper res step

    void . forkIO $ run (fromInteger p) (liftIO . serve tvar)

    infoL $ BS.pack "Time series available at http://0.0.0.0:"
        +++ p +++ BS.pack "/numbersd.{json,whisper}"

    runSink $ flush ^= \(k, v, ts, _) ->
        atomically . modifyTVar' tvar $ addMetric k v ts

serve :: TVar Whisper -> Request -> IO Response
serve tvar req = case rawPathInfo req of
    "/numbersd.json"    -> success jsonSeries
    "/numbersd.whisper" -> success textSeries
    _ -> response status404 $ copyByteString "{\"error\": \"Not Found\"}"
  where
    response c = return . ResponseBuilder c [("Content-Type", "text/plain")]
    success f  = do
        t <- currentTime
        w <- readTVarIO tvar
        response status200 $ f t w
