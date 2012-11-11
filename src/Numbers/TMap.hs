-- |
-- Module      : Numbers.TMap
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.TMap (
    -- * Opaque
      TMap
    , empty

    -- * Functions
    , toList
    , lookup
    , update
    , delete
    ) where

import Prelude                hiding (lookup)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Data.Maybe                    (fromJust)

import qualified Data.Map as M

newtype TMap k v = TMap (TVar (M.Map k v))

empty :: MonadIO m => m (TMap k v)
empty = TMap `liftM` atomic (newTVar M.empty)

toList :: MonadIO m => TMap k v -> m [(k, v)]
toList (TMap tvar) = M.toList `liftM` liftIO (readTVarIO tvar)

lookup :: (MonadIO m, Ord k) => k -> TMap k v -> m (Maybe v)
lookup key (TMap tvar) = M.lookup key `liftM` atomic (readTVar tvar)

update :: (MonadIO m, Ord k) => k -> (Maybe v -> m v) -> TMap k v -> m ()
update key f (TMap tvar) = do
    m <- atomic $ readTVar tvar
    v <- f $ M.lookup key m
    atomic $ writeTVar tvar $! M.insert key v m

delete :: (MonadIO m, Ord k) => k -> TMap k v -> m v
delete key (TMap tvar) = atomic $ do
    m <- readTVar tvar
    writeTVar tvar $! M.delete key m
    return . fromJust $ M.lookup key m

atomic :: MonadIO m => STM a -> m a
atomic = liftIO . atomically
