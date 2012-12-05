-- |
-- Module      : Numbers.Store
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Store (
      storeSink
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Data.Conduit             hiding (Flush)
import Numbers.Conduit
import Numbers.Types

import qualified Data.ByteString.Char8 as BS
import qualified Numbers.Map           as M

storeSink :: [Int]
          -> Int
          -> [EventSink]
          -> TBQueue BS.ByteString
          -> IO ()
storeSink qs n sinks q = runResourceT $ sourceQueue q $$ bracketP
    (liftIO . M.empty $ M.Continue n f)
    (\_ -> return ())
    (\m -> awaitForever $ liftIO . parse sinks m)
  where
    f k v ts = mapM_ (pushEvent sinks . Flush ts) $! calculate qs n k v

parse :: [EventSink] -> M.Map Key Metric -> BS.ByteString -> IO ()
parse sinks m bstr = forM_ (filter (not . BS.null) $ BS.lines bstr) f
  where
    f b = do
        pushEvent sinks $ Receive b
        measure "packets_received" m
        case decode metricParser b of
            Just (k, v) -> do
                measure "num_stats" m
                pushEvent sinks $ Parse k v
                insert k v m
            Nothing     -> do
                measure "bad_lines_seen" m
                pushEvent sinks $ Invalid b

measure :: Key -> M.Map Key Metric -> IO ()
measure = flip insert (Counter 1)

insert :: Key -> Metric -> M.Map Key Metric -> IO ()
insert key val = M.update key (return . aggregate val)
