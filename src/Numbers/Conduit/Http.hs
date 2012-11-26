-- |
-- Module      : Numbers.Conduit.Http
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Conduit.Http (
      httpSink
    ) where

import Blaze.ByteString.Builder hiding (flush)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Data.Aeson               hiding (json)
import Data.Maybe
import Data.Text.Encoding              (decodeUtf8)
import Network.Wai
import Network.Wai.Parse
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Numbers.Log
import Numbers.Types
import Numbers.Conduit.Internal

import qualified Control.Concurrent.STM.Map as M
import qualified Data.ByteString.Char8      as BS
import qualified Numbers.Whisper            as W

data ContentType = Json | Html | Text

data State = State
    { _whis  :: W.Whisper
    , _stats :: M.Map Key Int
    }

httpSink :: Int -> Int -> Maybe Int -> Maybe (IO EventSink)
httpSink res step = fmap $ \port -> do
    s <- M.empty
    w <- W.newWhisper res step

    async (run port $ liftIO . serve (State w s)) >>= link

    infoL $ "Serving /overview and /numbersd/<key> on http://0.0.0.0:" <&& port

    runSink . awaitForever $ \e -> case e of
        Flush ts p -> liftIO $ W.insert ts p w
        _          -> return ()

serve :: State -> Request -> IO Response
serve State{..} req | isNothing a = return unacceptable
                    | otherwise   = f
  where
    a = getType req
    f = case pathInfo req of
        ["numbersd"]      -> series b _whis
        ["numbersd", key] -> series b _whis
        ["overview"]      -> return $ overview b []
        _                 -> return $ unknown b
      where
        b = fromJust a

series :: ContentType -> W.Whisper -> IO Response
series typ whis = do
    ts <- currentTime
    response typ status200 `liftM` f ts ts whis
  where
    f = case typ of
        Json -> W.json
        _    -> W.text

getType :: Request -> Maybe ContentType
getType req = hAccept `lookup` requestHeaders req >>= f . parseHttpAccept
  where
    f h | p Json h  = return Json
        | p Html h  = return Text
        | p Text h  = return Text
        | otherwise = fail "Not Acceptable"
    p t = (pack t `elem`)

unacceptable :: Response
unacceptable = response Text status406 $ copyByteString "Not Acceptable"

unknown :: ContentType -> Response
unknown typ = response typ status404 $ copyByteString msg
  where
    msg = case typ of
        Json -> "{\"error\": \"Not Found\"}"
        _    -> "Not Found"

overview :: ContentType -> [(Key, Int)] -> Response
overview typ = response typ status200 . body
  where
    body = copyLazyByteString . encode . object . map f
    f (Key k, v) = decodeUtf8 k .= v

response :: ContentType -> Status -> Builder -> Response
response typ status = ResponseBuilder status [("Content-Type", pack typ)]

pack :: ContentType -> BS.ByteString
pack Json = "application/json"
pack Html = "text/html"
pack Text = "text/plain"
