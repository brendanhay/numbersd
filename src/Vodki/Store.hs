-- |
-- Module      : Vodki.Store
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Vodki.Store (
    -- * Eviction Handler
      Evicted

    -- * Opaque
    , Store
    , newStore

    -- * Functions
    , insert
    ) where

import Control.Applicative    ((<$>))
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe             (fromJust)
import Data.Time.Clock.POSIX
import Vodki.Metric

import qualified Data.Map as M

type Evicted a = Key -> a -> POSIXTime -> Int -> IO ()

type Map a = M.Map Key (TVar a)

data Store a = Store
    { _delay   :: Int
    , _evicted :: Evicted a
    , _tmap    :: TVar (Map a)
    }

newStore :: Int -> Evicted a -> IO (Store a)
newStore secs h = Store secs h <$> atomically (newTVar M.empty)

insert :: Metric a => Store a -> Key -> a -> IO ()
insert s@Store{..} key val = do
    m <- readTVarIO _tmap
    case M.lookup key m of
        Just v  -> atomically $ modifyTVar' v (append val)
        Nothing -> do
            atomically $ do
                v <- newTVar val
                writeTVar _tmap $! M.insert key v m
            timer s key

timer :: Metric a => Store a -> Key -> IO ()
timer s@Store{..} key = void . forkIO $ do
    threadDelay n'
    ts <- getPOSIXTime
    v  <- delete s key
    _evicted key v ts _delay
  where
    n' = _delay * 1000000

delete :: Metric a => Store a -> Key -> IO a
delete Store{..} key = atomically $ do
    m <- readTVar _tmap
    writeTVar _tmap $! M.delete key m
    swapTVar (fromJust $ M.lookup key m) empty
