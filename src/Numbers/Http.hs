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

import Blaze.ByteString.Builder       hiding (flush)
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Data.Aeson
import Data.Maybe
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Numbers.Log
import Numbers.Types
import Numbers.Conduit
import Numbers.Config

import qualified Data.HashMap.Strict as H
import qualified Numbers.Whisper     as W

data Format = Raw | Json

sinkHttp :: Config -> Maybe (IO EventSink)
sinkHttp conf = (flip fmap) (_httpPort conf) $ \port -> do
    w <- W.newWhisper (_resolution conf) (_interval conf)
    async (run port $ serve conf w) >>= link
    infoL $ "Serving /numbersd and /numbersd/render/<key> on http://0.0.0.0:" <&& port
    runSink . awaitForever $ \e -> case e of
        Aggregate p ts -> liftIO $ W.insert ts p w
        _              -> return ()

-- | Serves whispers as if served by graphite http://graphite.wikidot.com/url-api-reference
serve :: Config -> W.Whisper -> Application
serve conf whis req = case pathInfo req of
    ["numbersd", "config"] ->
        liftIO $ renderConfig fmt conf whis
    ["numbersd", "render"] -> liftIO $ do
        t <- currentTime
        renderSeries t t mts fmt whis
    ["numbersd"] ->
        static $ req { pathInfo = ["numbersd.html"] }
    _ ->
        static req
  where
    static = staticApp $ defaultWebAppSettings "assets"
    fmt    = getFormat req
    mts    = getTargets req

getFormat :: Request -> Format
getFormat req = case maybeParams format req of
    Just (x:_) -> x
    _          -> Raw
  where
    format ("format", Just "raw")  = Just Raw
    format ("format", Just "json") = Just Json
    format _                       = Nothing

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

success :: Builder -> Response
success = ResponseBuilder status200
    [ ("Content-Type", "text/plain")
    , ("Access-Control-Allow-Origin", "*")
    ]

renderSeries :: Time -> Time -> Maybe [Key] -> Format -> W.Whisper -> IO Response
renderSeries from to mks fmt whis = do
    ss <- W.fetch from to whis mks
    return . success $ f ss
  where
    f = case fmt of
        Raw  -> build . map (\(Key k, s) -> k &&> "," &&& s &&> "\n")
        Json -> copyLazyByteString . encode

renderConfig :: Format -> Config -> W.Whisper -> IO Response
renderConfig Raw  _    _    = return . success $ copyByteString ""
renderConfig Json conf whis = do
    ks <- W.keys whis
    return . success . copyLazyByteString . encode
        . Object $ H.insert "metrics" (toJSON ks) m
  where
    (Object m) = toJSON conf
