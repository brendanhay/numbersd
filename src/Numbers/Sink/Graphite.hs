-- |
-- Module      : Numbers.Sink.Graphite
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Sink.Graphite (
      graphiteSink
    ) where

import Data.Lens.Common
import Numbers.Log
import Numbers.Sink.Internal
import Numbers.Types

graphiteSink :: String -> Addr -> IO Sink
graphiteSink _ _ = runSink $
    flush ^= \(k, v, ts, _) -> infoL $ "Graphite: " +++ k ++& v ++& ts
