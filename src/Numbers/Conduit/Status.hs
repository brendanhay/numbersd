-- |
-- Module      : Numbers.Conduit.Status
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Conduit.Status (
      statusSink
    ) where

import Control.Monad.IO.Class
import Data.Conduit
import Numbers.Conduit.Internal
import Numbers.Store
import Numbers.Types

statusSink :: Store -> IO EventSink
statusSink store = runSink $ awaitForever f =$ sinkCounters store
  where
    f (Receive  _) = yield "packets_received"
    f (Invalid  _) = yield "bad_lines_seen"
    f (Parse  _ _) = yield "last_msg_seen"
    f _            = return ()

sinkCounters :: MonadIO m => Store -> Sink Key m ()
sinkCounters store = awaitForever (\k -> liftIO $ insert k (Counter 1) store)
