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
import Numbers.Conduit
import Numbers.Log
import Numbers.Sink.Internal
import Numbers.Types

broadcastSink :: Uri -> IO Sink
broadcastSink uri = do
    sock <- connect uri
    runSink $ receive ^= \b -> do
        infoL $ "Broadcast: " <&& b &&> " to " &&& uri
        send sock b
