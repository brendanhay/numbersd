-- |
-- Module      : Numbers.Sink.Status
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Sink.Status (
      statusSink
    ) where

import Control.Applicative    hiding (empty)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Data.Setters
import Data.String
import Data.Time.Clock.POSIX
import System.IO
import Numbers.Log
import Numbers.Metric
import Numbers.Sink
import Numbers.Socket

import qualified Data.ByteString.Char8 as BS

statusSink :: Addr -> IO Sink
statusSink addr = do
    listen addr
    runSink id

-- setFlush $ \k _ _ _ -> do
--         putStrLn $ "Status: " ++ show k ++ " on " ++ show addr

listen :: Addr -> IO ()
listen addr = do
    (s, a) <- openSocket addr Stream
    bindSocket s a
    void . forkIO . forever $ do
        (s', _) <- accept s
        respond s'
        sClose s'

respond :: Socket -> IO ()
respond sock = do
    sendAll sock $ BS.intercalate "\n"
        [ "HTTP/1.1 200 OK"
        , "some mad json yo!"
        ]
