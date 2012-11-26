-- |
-- Module      : Numbers.Conduit.Broadcast
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Conduit.Broadcast (
      broadcastSink
    ) where

import Numbers.Conduit.Internal
import Numbers.Types

broadcastSink :: Uri -> IO EventSink
broadcastSink uri = runSink $ awaitForever f =$ sinkSocket uri
  where
    f (Receive bs) = yield bs
    f _            = return ()
