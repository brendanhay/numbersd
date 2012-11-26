-- |
-- Module      : Numbers.Conduit.Log
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Conduit.Log (
      logSink
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.List              (intercalate)
import Numbers.Log
import Numbers.Conduit.Internal
import Numbers.Types

logSink :: [String] -> Maybe (IO EventSink)
logSink [] = Nothing
logSink es = Just $ do
    infoL $ "Logging " <&& intercalate ", " es &&> " events"
    runSink $ awaitForever f =$ awaitForever (liftIO . infoL)
  where
    f (Flush ts p) = yield $ p &&> " " &&& ts
    f _            = return ()



