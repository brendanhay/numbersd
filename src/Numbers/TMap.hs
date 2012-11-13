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
    , keys
    , lookup
    , insert
    , update
    , delete
    ) where

import Prelude                hiding (lookup)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM

import qualified Data.Map as M

newtype TMap k v = TMap (TVar (M.Map k v))

empty :: MonadIO m => m (TMap k v)
empty = TMap `liftM` atomic (newTVar M.empty)

toList :: MonadIO m => TMap k v -> m [(k, v)]
toList (TMap tvar) = M.toList `readVar` tvar

keys :: MonadIO m => TMap k v -> m [k]
keys (TMap tvar) = M.keys `readVar` tvar

lookup :: (MonadIO m, Ord k) => k -> TMap k v -> m (Maybe v)
lookup key (TMap tvar) = M.lookup key `readVar` tvar

insert :: (MonadIO m, Ord k) => k -> v -> TMap k v -> m ()
insert key val (TMap tvar) = atomic $ modifyTVar tvar (M.insert key val)

update :: (MonadIO m, Ord k) => k -> (Maybe v -> m v) -> TMap k v -> m ()
update key f (TMap tvar) = do
    m <- atomic $ readTVar tvar
    v <- f $ M.lookup key m
    atomic $ writeTVar tvar $! M.insert key v m

delete :: (MonadIO m, Ord k) => k -> TMap k v -> m (Maybe v)
delete key (TMap tvar) = atomic $ do
    (v, m) <- M.updateLookupWithKey del key `liftM` readTVar tvar
    writeTVar tvar m
    return v
  where
    del _ _ = Nothing

readVar :: MonadIO m => (a -> b) -> TVar a -> m b
readVar f tvar = f `liftM` atomic (readTVar tvar)

atomic :: MonadIO m => STM a -> m a
atomic = liftIO . atomically
