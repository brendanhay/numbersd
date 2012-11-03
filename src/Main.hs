-- |
-- Module      : Main
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main (
    -- * Entry Point
      main
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Vodki.Config
import Vodki.Network
import Vodki.Sink
import Vodki.Vodki

main :: IO ()
main = do
    Config{..} <- getConfig
    sinks <- sequence [dumpMessages, repeater "" 0, graphite "" 0]
    putStrLn "Sinks started..."
    sock <- listen _listenPort
    putStrLn "Listening..."
    runVodki _flushInterval sinks $ receive sock

listen :: Int -> IO Socket
listen port = do
    (s, a) <- openSocket Nothing port Datagram
    bindSocket s a
    return s

receive :: Socket -> Vodki ()
receive sock = forever $ do
    b <- liftIO $ recv sock 1024
    storeMetric b
