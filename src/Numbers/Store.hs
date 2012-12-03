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
    -- * Opaque
      Store
    , storeSink
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Data.Conduit             hiding (Flush)
import Numbers.Conduit.Internal
import Numbers.Types

import qualified Data.ByteString.Char8 as BS
import qualified Numbers.Map           as M

data Store = Store
    { _sinks :: [EventSink]
    , _tmap  :: M.Map Key Metric
    }

storeSink :: [Int]
          -> Int
          -> [EventSink]
          -> TBQueue BS.ByteString
          -> IO ()
storeSink qs n sinks q = runResourceT $ sourceQueue q $$ bracketP
    (liftIO $ newStore qs n sinks)
    (\_ -> return ())
    (\s -> awaitForever $ liftIO . flip parse s)

newStore :: [Int] -> Int -> [EventSink] -> IO Store
newStore qs n sinks = Store sinks `fmap` M.empty (M.Continue n f)
  where
    f k m ts = mapM_ (pushEvent sinks . Flush ts) $ calculate qs n k m

parse :: BS.ByteString -> Store -> IO ()
parse bstr Store{..} = do
    pushEvent _sinks $ Receive bstr
    measure "packets_received" _tmap
    forM_ (filter (not . BS.null) $ BS.lines bstr) f
  where
    f b = case decode metricParser b of
        Just (k, v) -> do
            measure "num_stats" _tmap
            insert k v _tmap
        Nothing     -> do
            measure "bad_lines_seen" _tmap
            pushEvent _sinks $ Invalid bstr

measure :: Key -> M.Map Key Metric -> IO ()
measure = flip insert (Counter 1)

insert :: Key -> Metric -> M.Map Key Metric -> IO ()
insert key val = M.update key (return . aggregate val)
