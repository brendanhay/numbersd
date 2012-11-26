-- |
-- Module      : Numbers.Conduit.Graphite
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Conduit.Graphite (
      graphiteSink
    ) where

import Numbers.Conduit.Internal
import Numbers.Types

graphiteSink :: String -> Uri -> IO EventSink
graphiteSink pref uri = runSink $ awaitForever f =$ sinkSocket uri
  where
    f (Flush ts p) = yield "ballsacks 1.0 123123123"
    f _            = return ()
