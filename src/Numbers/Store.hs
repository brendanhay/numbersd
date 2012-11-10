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
    , insert
    ) where

import Control.Applicative    hiding (empty)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import Data.Time.Clock.POSIX
import Numbers.Sink
import Numbers.Types
import

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M

data Store = Store
    { interval :: Int
    , sinks    :: [Sink]
    , store    :: TVar (M.Map Key (TVar Metric))
    }

newStore :: Int -> [Sink] -> IO Store
newStore n sinks = Store n sinks <$> atomically (newTVar M.empty)


insert :: Store -> BS.ByteString -> IO ()
insert s@Store{..} bstr = do
    emit sinks $ Receive bstr
    forM_ (filter (not . BS.null) $ BS.lines bstr) f
  where
    f b = case decode metric b of
        Just (k, v) -> bucket s k v
        Nothing     -> emit sinks $ Invalid bstr

bucket :: Store -> Key -> Metric -> IO ()
bucket s@Store{..} key val = do
    emit sinks $ Parse key val
    m <- readTVarIO store
    case M.lookup key m of
        Just v  -> atomically $ modifyTVar' v (aggregate val)
        Nothing -> do
            atomically $ do
                v <- newTVar val
                writeTVar store $! M.insert key v m
            flush s key

flush :: Store -> Key -> IO ()
flush s@Store{..} key = void . forkIO $ do
    threadDelay n
    v  <- delete s key
    ts <- getPOSIXTime
    emit sinks $ Flush key v ts interval
  where
    n = interval * 1000000

delete :: Store -> Key -> IO Metric
delete Store{..} key = atomically $ do
    m <- readTVar store
    writeTVar store $! M.delete key m
    readTVar (fromJust $ M.lookup key m)