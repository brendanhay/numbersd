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

module Numbers.Map (
    -- * Opaque
      Map
    , Policy(..)
    , empty

    -- * Functions
    , toList
    , keys
    , lookup
    , update
    ) where

import Prelude                  hiding (lookup)
import Control.Arrow                   (second)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async hiding (wait)
import Control.Concurrent.STM
import Numbers.Types            hiding (P)

import qualified Data.Map as M

data Entry v =
    Transient
    { _expire :: Time
    , _value  :: v
    }
  | Permanent
    { _value  :: v
    }

type Handler k v = k -> v -> Time -> IO ()

data Policy k v = Reset Int (Handler k v)
                | Continue Int (Handler k v)
                | NoPolicy

type TMap k v = TVar (M.Map k (Entry v))

data Map k v = Map
    { _policy :: Policy k v
    , _tmap   :: TMap k v
    }

empty :: Policy k v -> MonadIO m => m (Map k v)
empty p = Map p `liftM` atomic (newTVar M.empty)

toList :: MonadIO m => Map k v -> m [(k, v)]
toList Map{..} = map (second _value) `liftM` (M.toList `atomicRead` _tmap)

keys :: MonadIO m => Map k v -> m [k]
keys Map{..} = M.keys `atomicRead` _tmap

lookup :: (MonadIO m, Ord k) => k -> Map k v -> m (Maybe v)
lookup key Map{..} = do
    m <- M.lookup key `atomicRead` _tmap
    return $ _value `fmap` m

update :: (MonadIO m, Ord k) => k -> (Maybe v -> m v) -> Map k v -> m ()
update key f tm@Map{..} = do
    e  <- M.lookup key `atomicRead` _tmap
    v  <- f $ _value `fmap` e
    maybe (insert key v tm) (existing key v tm) e

insert :: (MonadIO m, Ord k) => k -> v -> Map k v -> m ()
insert key val Map{..} = do
    e <- case _policy of
        Reset n f    -> g f n
        Continue n f -> g f n
        NoPolicy     -> return $ Permanent val
    atomic $ modifyTVar' _tmap (M.insert key e)
  where
    g = sweep key val _tmap

existing :: (MonadIO m, Ord k) => k -> v -> Map k v -> Entry v -> m ()
existing key val Map{..} e =
    atomic $ modifyTVar' _tmap (M.insert key $ e { _value = val })

sweep :: (MonadIO m, Ord k) => k -> v -> TMap k v -> Handler k v -> Int -> m (Entry v)
sweep key val tmap f n = do
    liftIO $ async (wait n) >>= link
    ts <- liftIO $ (+ fromIntegral n) `liftM` currentTime
    return $ Transient ts val
  where
    wait d = do
        ts <- liftIO $ threadDelay d >> currentTime
        v  <- M.lookup key `atomicRead` tmap
        maybe (delete key tmap >> f key val ts) wait (delay v ts)

delete :: (MonadIO m, Ord k) => k -> TMap k v -> m ()
delete key tmap = atomic $! do
    m <- M.delete key `liftM` readTVar tmap
    writeTVar tmap m

delay :: Maybe (Entry v) -> Time -> Maybe Int
delay (Just (Transient t _)) ts
    | d > 0     = Just d
    | otherwise = Nothing
  where
    d = fromIntegral $ ts - t
delay _ _ = Nothing

atomicRead :: MonadIO m => (a -> b) -> TVar a -> m b
atomicRead f tvar = f `liftM` atomic (readTVar tvar)

atomic :: MonadIO m => STM a -> m a
atomic = liftIO . atomically
