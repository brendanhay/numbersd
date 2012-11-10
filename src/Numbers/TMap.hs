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
import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Map as M

newtype TMap k v = TMap { tvar :: TVar (M.TMap k v) }

newTMap :: MonadIO m => m (TMap k v)
newTMap = TMap <$> newTVar M.empty

updateTMap :: (MonadIO m, Ord k) => TMap k v -> k -> (Maybe v -> v) -> m ()
updateTMap key f = modifyTVar' tmap (f . M.lookup key)

deleteTMap :: (MonadIO m, Ord k) => TMap k v -> k -> m v
deleteTMap tmap key = do
    m <- readTVar tmap
    writeTVar tmap $! M.delete key m
    return . fromJust $ M.lookup key m