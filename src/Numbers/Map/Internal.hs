-- |
-- Module      : Numbers.Map
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Map.Internal (
    -- * Exported Types
      Entry(..)
    , Update(..)

    -- * Opaque
    , Map
    , empty

    -- * Functions
    , toList
    , keys
    , lookup
    , update
    , deleteIf
    ) where

import Prelude                  hiding (lookup)
import Control.Applicative             ((<$>))
import Control.Arrow                   (second)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Numbers.Types            hiding (P)

import qualified Data.Map as M

data Entry v = Entry {
    create :: Time
  , modify :: Time
  , value :: v
}

type Map k v = TVar (M.Map k (Entry v))

data Update = New | Existing
  deriving (Eq, Show)

empty :: MonadIO m => m (Map k v)
empty = atomic $ newTVar M.empty

toList :: MonadIO m => Map k v -> m [(k, v)]
toList tm = map (second value) `liftM` (M.toList `atomicRead` tm)

keys :: MonadIO m => Map k v -> m [k]
keys tm = M.keys `atomicRead` tm

lookup :: (MonadIO m, Ord k) => k -> Map k v -> m (Maybe v)
lookup key tm = do
  m <- M.lookup key `atomicRead` tm
  return $ value <$> m

update :: (MonadIO m, Ord k) => k -> (Maybe v -> v) -> Map k v -> m Update
update key f tm = do
  now <- liftIO currentTime
  atomic $! do
    me <- M.lookup key <$> readTVar tm
    let val = f $ fmap value me
        (u, e') = maybe (New, Entry now now val) (\e -> (Existing, e{modify = now, value = val})) me
    modifyTVar' tm $ M.insert key e'
    return u

deleteIf :: (MonadIO m, Ord k) => (Entry v -> Bool) -> k -> Map k v -> m (Maybe (Entry v))
deleteIf f key tm = atomic $! do
  me <- M.lookup key <$> readTVar tm
  case f <$> me of
    Just True -> do
      modifyTVar' tm $ M.delete key
      return me
    _ -> return Nothing

atomicRead :: MonadIO m => (a -> b) -> TVar a -> m b
atomicRead f tm = f `liftM` atomic (readTVar tm)

atomic :: MonadIO m => STM a -> m a
atomic = liftIO . atomically
