-- |
-- Module      : Numbers.Sink
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Sink (
    -- * Events
      Event(..)

    -- * Opaque
    , Sink

    -- * Sinks
    , logSink
    , seriesSink
    , graphiteSink
    , broadcastSink
    , downstreamSink

    -- * Functions
    , emit
    ) where

import Numbers.Sink.Internal
import Numbers.Sink.Log
import Numbers.Sink.Series
import Numbers.Sink.Graphite
import Numbers.Sink.Broadcast
import Numbers.Sink.Downstream
