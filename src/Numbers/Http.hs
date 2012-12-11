{-# LANGUAGE TemplateHaskell #-}
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
import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Data.FileEmbed
import Data.Maybe
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Numbers.Log
import Numbers.Types
import Numbers.Conduit

import qualified Data.ByteString.Char8 as BS
import qualified Numbers.Whisper       as W

sinkHttp :: Int -> Int -> Maybe Int -> Maybe (IO EventSink)
sinkHttp res step = fmap $ \port -> do
    w <- W.newWhisper res step
    async (run port $ liftIO . serve w) >>= link
    infoL $ "Serving /numbersd and /numbersd/render/<key> on http://0.0.0.0:" <&& port
    runSink . awaitForever $ \e -> case e of
        Aggregate p ts -> liftIO $ W.insert ts p w
        _              -> return ()

-- | Serves whispers as if served by graphite http://graphite.wikidot.com/url-api-reference
serve :: W.Whisper -> Request -> IO Response
serve whis req = case pathInfo req of
    ["numbersd"]           -> return index
    ["numbersd", "render"] -> series whis $ getTargets req
    ["javascripts.js"]     -> return javascripts
    _                      -> return unknown

series :: W.Whisper -> Maybe [Key] -> IO Response
series whis mks = do
    now <- currentTime
    success "text/plain" <$> W.raw now now whis mks

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

success :: BS.ByteString -> Builder -> Response
success = response status200

unknown :: Response
unknown = response status404 "text/plain" $ copyByteString "Error: Not Found"

response :: Status -> BS.ByteString -> Builder -> Response
response status content = ResponseBuilder status
    [ ("Content-Type", content)
    , ("Access-Control-Allow-Origin", "*")
    ]

index :: Response
index = file "text/html" $(embedFile "assets/index.html")

javascripts :: Response
javascripts = file "application/javascript" $(embedFile "assets/javascripts.js")

file :: BS.ByteString -> BS.ByteString -> Response
file content = success content . copyByteString
