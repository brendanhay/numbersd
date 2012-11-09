{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

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

import Blaze.ByteString.Builder  (Builder, copyByteString, fromLazyByteString)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Data.Aeson
import Data.Lens.Common
import Data.Lens.Template
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types        (status200, status404)
import Network.HTTP.Types.Status (Status)
import Numbers.Log
import Numbers.Types
import Numbers.Sink.Internal

import Data.Text.Encoding                (decodeUtf8)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M

newtype Map = Map (M.Map Key Metric)

instance ToJSON Map where
    toJSON (Map m) = object . map f $ M.toAscList m
      where
        f (Key k, v) = decodeUtf8 k .= object
            [ "value" .= v
            , "timestamp" .= decodeUtf8 "0"
            ]

data Overview = Overview
    { _counters :: Map
    , _timers   :: Map
    , _gauges   :: Map
    , _sets     :: Map
    }

instance ToJSON Overview where
    toJSON Overview{..} = object
        [ "counters" .= _counters
        , "timers"   .= _timers
        , "gauges"   .= _gauges
        , "sets"     .= _sets
        ]

$(makeLens ''Overview)

overviewSink :: Maybe Int -> Maybe (IO Sink)
overviewSink = fmap $ \p -> do
    tvar <- newOverview
    void . forkIO $ run p (liftIO . serve tvar)
    infoL $ BS.pack "Overview available at http://0.0.0.0:" +++ p +++ page
    runSink $ flush ^= \(k, v, _, _) ->
        atomically . modifyTVar tvar $ add k v

newOverview :: IO (TVar Overview)
newOverview = atomically . newTVar $ Overview m m m m
  where
    m = Map M.empty

add :: Key -> Metric -> Overview -> Overview
add key val = l $ insert key val
  where
    l = modL $ case val of
        (Counter _) -> counters
        (Timer _)   -> timers
        (Gauge _)   -> gauges
        (Set _)     -> sets

insert :: Key -> Metric -> Map -> Map
insert key val (Map inner) = Map $! M.alter (const $ Just val) key inner

serve :: TVar Overview -> Request -> IO Response
serve tvar req | rawPathInfo req == page = success `liftM` readTVarIO tvar
               | otherwise               = return notFound

page :: BS.ByteString
page = "/numbersd.json"

success :: ToJSON a => a -> Response
success = response status200 . fromLazyByteString . encode

notFound :: Response
notFound = response status404 $ copyByteString "{\"error\": \"Not Found\"}"

response :: Status -> Builder -> Response
response code = ResponseBuilder code [("Content-Type", "application/json")]
