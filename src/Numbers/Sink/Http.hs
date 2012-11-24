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
import Data.Aeson               hiding (json)
import Data.Lens.Common                ((^=))
import Data.Text.Encoding              (decodeUtf8)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types              (Status, status200, status404)
import Numbers.Log
import Numbers.Types
import Numbers.Sink.Internal

import qualified Control.Concurrent.STM.Map as M
import qualified Data.ByteString.Char8      as BS
import qualified Numbers.Whisper            as W

data State = State
    { _whis  :: W.Whisper
    , _stats :: M.Map Key Int
    }

httpSink :: [Int]         -- ^ Quantiles
         -> Int           -- ^ Resolution
         -> Int           -- ^ Step
         -> BS.ByteString -- ^ Prefix
         -> Maybe Int     -- ^ Port
         -> Maybe (IO Sink)
httpSink qs res step pref = fmap $ \port -> do
    s <- M.empty
    w <- W.newWhisper qs res step pref

    -- Start the HTTP server
    async (run port $ liftIO . serve (State w s)) >>= link

    infoL $ "Serving http://0.0.0.0:" <&& port
        &&> "/overview.json, and /numbersd.{json,whisper}"

    -- Start a thread for flush events
    runSink $ flush ^= \(k, v, ts, _) ->
        -- TODO: Work on internal counters and overview
        -- Store time series
        W.insert k v ts w

serve :: State -> Request -> IO Response
serve State{..} req = case rawPathInfo req of
    "/overview.json"    -> return $ overview []
    "/numbersd.json"    -> series (W.json $ Time 0) jsonType
    "/numbersd.whisper" -> series (W.text $ Time 0) whisperType
    _                   -> unknown
  where
    series f typ = do
        t <- currentTime
        response status200 typ `liftM` f t _whis
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