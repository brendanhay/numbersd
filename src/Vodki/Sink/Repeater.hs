-- |
-- Module      : Vodki.Sink.Repeater
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Vodki.Sink.Repeater (
      repeaterSink
    ) where

import Vodki.Sink.Internal
import Vodki.Socket

import qualified Data.ByteString.Char8 as BS

repeaterSink :: Addr -> IO Sink
repeaterSink addr = do
    r <- openSocketR addr Datagram
    putStrLn $ "Repeater connected to " ++ show addr
    runSink . setReceive $ \s -> do
        putStrLn $ "Repeat: " ++ BS.unpack s ++ " to " ++ show addr
        sendR r s
