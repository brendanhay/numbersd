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

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List              (intercalate)
import Numbers.Conduit.Internal
import Numbers.Log
import Numbers.Types

logSink :: [String] -> Maybe (IO EventSink)
logSink [] = Nothing
logSink es = Just $ do
    infoL $ "Logging '" <&& intercalate ", " es &&> "' events"
    runSink $ awaitForever (f . g)
  where
    f (k, v) = when (k `elem` es) (liftIO $ infoL v)
    g (Receive bs) = ("receive", "Receive: " <&& bs)
    g (Invalid bs) = ("invalid", "Invalid: " <&& bs)
    g (Parse k v)  = ("parse", "Parse: " <&& k &&> " " &&& v)
    g (Flush ts p) = ("flush", "Flush: "   <&& p &&> " " &&& ts)
