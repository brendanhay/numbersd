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
    , newStore

    -- * Functions
    , parse
    ) where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Numbers.Sink
import Numbers.Types

import qualified Control.Concurrent.STM.Map as M
import qualified Data.ByteString.Char8      as BS

data Store = Store
    { _percentiles :: [Int]
    , _interval    :: Int
    , _sinks       :: [Sink]
    , _tmap        :: M.Map Key Metric
    }

newStore :: [Int] -> Int -> [Sink] -> IO Store
newStore qs n sinks = Store qs n sinks `fmap` M.empty

parse :: BS.ByteString -> Store -> IO ()
parse bstr s@Store{..} = do
    emit _sinks $ Receive bstr
    forM_ (filter (not . BS.null) $ BS.lines bstr) f
  where
    f b = case decode metricParser b of
        Just (k, v) -> bucket k v s
        Nothing     -> emit _sinks $ Invalid bstr

bucket :: Key -> Metric -> Store -> IO ()
bucket key val s@Store{..} = M.update key f _tmap
  where
    f (Just x) = return $ x `aggregate` val
    f Nothing  = flush key s >>= link >> return val

flush :: Key -> Store -> IO (Async ())
flush key Store{..} = async $ do
    threadDelay $ _interval * 1000000
    M.delete key _tmap >>= f
  where
    f Nothing  = return ()
    f (Just v) = do
        ts <- currentTime
        mapM_ (emit _sinks . Flush ts)
            $ calculate _percentiles _interval key v
