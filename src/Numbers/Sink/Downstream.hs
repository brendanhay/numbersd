-- |
-- Module      : Numbers.Sink.Downstream
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Sink.Downstream (
      downstreamSink
    ) where

import Data.Lens.Common
import Numbers.Log
import Numbers.Sink.Internal
import Numbers.Types

downstreamSink :: Addr -> IO Sink
downstreamSink _ = runSink $
    flush ^= \(k, v, ts, _) -> infoL $ "Upstream: " +++ k ++& v ++& ts
