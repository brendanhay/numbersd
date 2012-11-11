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
import Control.Concurrent
import Control.Concurrent.STM
import Data.Aeson
import Data.Lens.Common                ((^=))
import Data.Text.Encoding              (decodeUtf8)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types              (Status, status200, status404)
import Numbers.Log
import Numbers.Types
import Numbers.Sink.Internal
import Numbers.Whisper

import qualified Data.ByteString.Char8 as BS
import qualified Numbers.TMap          as M

data State = State
    { _whisper :: TVar Whisper
    , _stats   :: M.TMap Key Int
    }

httpSink :: Resolution -> Step -> Maybe Integer -> Maybe (IO Sink)
httpSink res step = fmap $ \p -> do
    whis  <- atomically . newTVar $ newWhisper res step
    stats <- M.empty

    void . forkIO $ run (fromInteger p) (liftIO . serve (State whis stats))

    infoL $ BS.pack "Serving http://0.0.0.0:"
        +++ p +++ BS.pack "overview.json, and /numbersd.{json,whisper}"

    runSink $ flush ^= \(k, v, ts, _) -> do
        M.update k
             (\n -> return $ case n of
                   Just x  -> x + 1 :: Int
                   Nothing -> 1)
             stats

        atomically . modifyTVar' whis $ addMetric k v ts

serve :: State -> Request -> IO Response
serve State{..} req = case rawPathInfo req of
    "/overview.json"    -> overview `liftM` M.toList _stats
    "/numbersd.json"    -> series _whisper jsonSeries jsonType
    "/numbersd.whisper" -> series _whisper textSeries whisperType
    _                   -> return unknown

overview :: [(Key, Int)] -> Response
overview = response status200 jsonType . body
  where
    body = copyLazyByteString . encode . object . map f
    f (Key k, v) = decodeUtf8 k .= v

series :: TVar a -> (Time -> a -> Builder) -> BS.ByteString -> IO Response
series tvar f typ = do
    t <- currentTime
    w <- readTVarIO tvar
    return . response status200 typ $ f t w

unknown :: Response
unknown = response status404 jsonType
    $ copyByteString "{\"error\": \"Not Found\"}"

response :: Status -> BS.ByteString -> Builder -> Response
response status typ = ResponseBuilder status [("Content-Type", typ)]

jsonType :: BS.ByteString
jsonType = "application/json"

whisperType :: BS.ByteString
whisperType = "text/plain"