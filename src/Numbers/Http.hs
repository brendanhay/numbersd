-- |
-- Module      : Numbers.Http
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Http (
      sinkHttp
    ) where

import Blaze.ByteString.Builder hiding (flush)
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Data.Maybe
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Numbers.Log
import Numbers.Types
import Numbers.Conduit

import qualified Numbers.Whisper as W

data Format = Raw | Pretty

sinkHttp :: Int -> Int -> Maybe Int -> Maybe (IO EventSink)
sinkHttp res step = fmap $ \port -> do
    w <- W.newWhisper res step
    async (run port $ liftIO . serve w) >>= link
    infoL $ "Serving /numbersd and /numbersd/render/<key> on http://0.0.0.0:" <&& port
    runSink . awaitForever $ \e -> case e of
        Aggregate p ts -> liftIO $ W.insert ts p w
        _              -> return ()

-- | serves whispers as if served by graphite http://graphite.wikidot.com/url-api-reference
serve :: W.Whisper -> Request -> IO Response
serve whis req = case pathInfo req of
    ("numbersd":_) -> series whis (getFormat req) (getTargets req)
    _              -> return unknown

series :: W.Whisper -> Format -> Maybe [Key] -> IO Response
series whis fmt mks = do
    now  <- currentTime
    body <- case fmt of
        Pretty -> W.raw now now whis mks
        Raw    -> W.raw now now whis mks
    return $ response status200 body

getFormat :: Request -> Format
getFormat req = case maybeParams fmt req of
    Just (f:_) -> f
    _          -> Pretty
  where
    fmt ("format", Just "raw") = Just Raw
    fmt _                      = Nothing

getTargets :: Request -> Maybe [Key]
getTargets = maybeParams target
  where
    target ("target", Just x) = Just $ Key x
    target _                  = Nothing

maybeParams :: (QueryItem -> Maybe a) -> Request -> Maybe [a]
maybeParams f = g . catMaybes . map f . queryString
  where
    g [] = Nothing
    g xs = Just xs

unknown :: Response
unknown = response status404 $ copyByteString "Error: Not Found"

response :: Status -> Builder -> Response
response status = ResponseBuilder status
    [ ("Content-Type", "text/plain")
    , ("Access-Control-Allow-Origin", "*")
    ]
