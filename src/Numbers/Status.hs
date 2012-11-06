-- |
-- Module      : Numbers.Status
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Status (
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
import Numbers.Socket
import Numbers.Sink

import qualified Data.ByteString.Char8 as BS

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types       (status200)
import Blaze.ByteString.Builder (copyByteString)
import Data.Monoid

statusSink :: Addr -> IO Sink
statusSink (Addr _ port) = do
    forkIO $ run port app

    runSink . setFlush $ \s -> do
        putStrLn $ "Status: " ++ BS.unpack s ++ " to " ++ show addr

app req = return builderNoLen

builderNoLen = ResponseBuilder
    status200
    [ ("Content-Type", "text/plain")
    ]
    $ copyByteString "PONG"
