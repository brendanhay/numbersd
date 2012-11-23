-- |
-- Module      : Numbers.Sink.Log
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Sink.Log (
      logSink
    ) where

import Data.List        (intercalate)
import Data.Lens.Common
import Numbers.Log
import Numbers.Sink.Internal
import Numbers.Types

logSink :: [String] -> Maybe (IO Sink)
logSink []   = Nothing
logSink evts = Just $ do
    infoL $ "Logging " <&& (intercalate ", " evts) &&> " events"
    runSink $ \s -> foldl f s (ts infoL)
  where
    f s (k, g) = if k `elem` evts then g s else s
    ts l = [ ("receive", receive ^= \v -> l $ "Receive: " <&& v)
           , ("invalid", invalid ^= \v -> l $ "Invalid: " <&& v)
           , ("parse", parse   ^= \(k, v) -> l $ "Parse: " <&& k &&& " " <&& v)
           , ("flush", flush   ^= \(k, v, _, _) -> l $ "Flush: " <&& k &&& " " <&& v)
           ]
