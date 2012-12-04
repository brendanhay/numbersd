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

import Blaze.ByteString.Builder (toByteString)
import Numbers.Conduit.Internal
import Numbers.Log
import Numbers.Types

graphiteSink :: String -> Uri -> IO EventSink
graphiteSink pref uri = do
    infoL $ "Connected to graphite " <&& uri
    runSink $ awaitForever f =$ sinkUri uri
  where
    f (Flush ts p) = yield . toByteString $ pref &&> "." &&& p &&> " " &&& ts &&> "\n"
    f _            = return ()

