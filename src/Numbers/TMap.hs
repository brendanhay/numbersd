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

module Numbers.TMap where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Data.Maybe             (fromJust)

import qualified Data.Map as M

newtype TMap k v = TMap (TVar (M.Map k v))

newTMap :: MonadIO m => m (TMap k v)
newTMap = TMap `liftM` (atomic $ newTVar M.empty)

updateTMap :: (MonadIO m, Ord k) => TMap k v -> k -> (Maybe v -> m v) -> m ()
updateTMap (TMap tvar) key f = do
    m <- atomic $ readTVar tvar
    v <- f $ M.lookup key m
    atomic $ writeTVar tvar $! M.insert key v m

deleteTMap :: (MonadIO m, Ord k) => TMap k v -> k -> m v
deleteTMap (TMap tvar) key = atomic $ do
    m <- readTVar tvar
    writeTVar tvar $! M.delete key m
    return . fromJust $ M.lookup key m

atomic :: MonadIO m => STM a -> m a
atomic = liftIO . atomically
