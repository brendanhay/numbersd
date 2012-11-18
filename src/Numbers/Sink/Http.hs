{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Numbers.Sink.Http
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Sink.Http (
      httpSink
    ) where

import Blaze.ByteString.Builder hiding (flush)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Aeson               hiding (json)
import Data.Lens.Common                ((^=))
import Data.Text.Encoding              (decodeUtf8)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types              (Status, status200, status404)
import Numbers.Log
import Numbers.Types
import Numbers.Sink.Internal

import qualified Data.ByteString.Char8 as BS
import qualified Numbers.TMap          as M
import qualified Numbers.Whisper       as W

data State = State
    { _whisper :: TVar W.Whisper
    , _stats   :: M.TMap Key Int
    }

httpSink :: Int -> Int -> Maybe Int -> Maybe (IO Sink)
httpSink res step = fmap $ \p -> do
    whis  <- atomically . newTVar $ W.newWhisper res step
    stats <- M.empty

    -- Start the HTTP server
    a <- async $ run p (liftIO . serve (State whis stats))
    link a

    infoL $ BS.pack "Serving http://0.0.0.0:"
        +++ p +++ BS.pack "/overview.json, and /numbersd.{json,whisper}"

    -- Start a thread for flush events
    runSink $ flush ^= \(k, v, ts, _) -> do

--      \\ Work on internal counters and overview

        -- Update counters
        M.update k
             (\n -> return $ case n of
                   Just x  -> x + 1 :: Int
                   Nothing -> 1)
             stats

        -- Store time series
        atomically . modifyTVar' whis $ W.insert k v ts

serve :: State -> Request -> IO Response
serve State{..} req = case rawPathInfo req of
    "/overview.json"    -> overview `liftM` M.toList _stats
    "/numbersd.json"    -> series (W.json $ Time 0) jsonType
    "/numbersd.whisper" -> series (W.text $ Time 0) whisperType
    _                   -> unknown
  where
    series f typ = do
        t <- currentTime
        w <- readTVarIO _whisper
        return . response status200 typ $ f t w
    unknown = return . response status404 jsonType
        $ copyByteString "{\"error\": \"Not Found\"}"

overview :: [(Key, Int)] -> Response
overview = response status200 jsonType . body
  where
    body = copyLazyByteString . encode . object . map f
    f (Key k, v) = decodeUtf8 k .= v

response :: Status -> BS.ByteString -> Builder -> Response
response status typ = ResponseBuilder status [("Content-Type", typ)]

jsonType :: BS.ByteString
jsonType = "application/json"

whisperType :: BS.ByteString
whisperType = "text/plain"