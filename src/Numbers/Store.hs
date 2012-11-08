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
      Store
    , runStore
    , storeMetric
    ) where

import Control.Applicative    hiding (empty)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import Data.Time.Clock.POSIX
import Numbers.Metric
import Numbers.Sink

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M

data State = State
    { interval :: Int
    , sinks    :: [Sink]
    , store    :: TVar (M.Map Key (TVar Metric))
    }

type Store a = ReaderT State IO a

runStore :: Int -> [Sink] -> Store a -> IO a
runStore n sinks vodki = do
    s <- State n sinks <$> atomically (newTVar M.empty)
    runReaderT vodki s

storeMetric :: BS.ByteString -> Store ()
storeMetric bstr = ask >>= liftIO . flip insert bstr

insert :: State -> BS.ByteString -> IO ()
insert s@State{..} bstr = do
    emit sinks $ Receive bstr
    forM_ (filter (not . BS.null) $ BS.lines bstr) f
  where
    f b = case decode b of
        Just (k, v) -> bucket s k v
        Nothing     -> emit sinks $ Invalid bstr

bucket :: State -> Key -> Metric -> IO ()
bucket s@State{..} key val = do
    emit sinks $ Parse key val
    m <- readTVarIO store
    case M.lookup key m of
        Just v  -> atomically $ modifyTVar' v (append val)
        Nothing -> do
            atomically $ do
                v <- newTVar val
                writeTVar store $! M.insert key v m
            flush s key

flush :: State -> Key -> IO ()
flush s@State{..} key = void . forkIO $ do
    threadDelay n
    v  <- delete s key
    ts <- getPOSIXTime
    emit sinks $ Flush key v ts interval
  where
    n = interval * 1000000

delete :: State -> Key -> IO Metric
delete State{..} key = atomically $ do
    m <- readTVar store
    writeTVar store $! M.delete key m
    readTVar (fromJust $ M.lookup key m)