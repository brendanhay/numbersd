-- |
-- Module      : Numbers.Sink.Broadcast
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Sink.Broadcast (
      broadcastSink
    ) where

import Data.Lens.Common
import Numbers.Log
import Numbers.Sink.Internal
import Numbers.Socket
import Numbers.Types

broadcastSink :: Addr -> IO Sink
broadcastSink addr = do
    r <- openSocketR addr Datagram
    newSink $ receive ^= \b -> do
        infoL $ "Broadcast: " +++ b +++ " to " +++ addr
        sendR r b
