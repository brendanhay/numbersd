-- |
-- Module      : Control.Concurrent.STM.Map
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Control.Concurrent.STM.Map (
    -- * Opaque
      Map
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

newtype Map k v = Map (TVar (M.Map k v))

empty :: MonadIO m => m (Map k v)
empty = Map `liftM` atomic (newTVar M.empty)

toList :: MonadIO m => Map k v -> m [(k, v)]
toList (Map tvar) = M.toList `readVar` tvar

keys :: MonadIO m => Map k v -> m [k]
keys (Map tvar) = M.keys `readVar` tvar

lookup :: (MonadIO m, Ord k) => k -> Map k v -> m (Maybe v)
lookup key (Map tvar) = M.lookup key `readVar` tvar

insert :: (MonadIO m, Ord k) => k -> v -> Map k v -> m ()
insert key val (Map tvar) = atomic $ modifyTVar tvar (M.insert key val)

update :: (MonadIO m, Ord k) => k -> (Maybe v -> m v) -> Map k v -> m ()
update key f (Map tvar) = do
    m <- atomic $ readTVar tvar
    v <- f $ M.lookup key m
    atomic $ writeTVar tvar $! M.insert key v m

delete :: (MonadIO m, Ord k) => k -> Map k v -> m (Maybe v)
delete key (Map tvar) = atomic $ do
    (v, m) <- M.updateLookupWithKey (\_ _ -> Nothing) key `liftM` readTVar tvar
    writeTVar tvar m
    return v

readVar :: MonadIO m => (a -> b) -> TVar a -> m b
readVar f tvar = f `liftM` atomic (readTVar tvar)

atomic :: MonadIO m => STM a -> m a
atomic = liftIO . atomically
