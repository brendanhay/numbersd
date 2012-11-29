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
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Traversable                (traverse)
import Numbers.Types            hiding (P)

import qualified Data.Map as M

data Entry v = T { _expire :: Time, _value :: v } | P { _value :: v }

type Handler k v = k -> v -> Time -> IO ()

data Policy k v = Reset Int (Handler k v)
                | Continue Int (Handler k v)
                | Permanent

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
    m  <- atomic $ readTVar _tmap
    let e = M.lookup key m
    v  <- f $ _value `fmap` e
    case e of
        Just x  -> atomic . writeTVar _tmap $! M.insert key (x { _value = v }) m
        Nothing -> insert key v tm

insert :: (MonadIO m, Ord k) => k -> v -> Map k v -> m ()
insert key val Map{..} = do
    e <- entry _policy key val _tmap
    atomic $ modifyTVar _tmap (M.insert key e)

entry :: (MonadIO m, Ord k) => Policy k v -> k -> v -> TMap k v -> m (Entry v)
entry p key val tmap = case p of
    Reset n h    -> f n h
    Continue n h -> f n h
    Permanent    -> return $ P val
  where
    f n h = do
        ts <- liftIO $ (+ fromIntegral n) `liftM` currentTime
        sweep key h (n * 1000000) tmap
        return $ T ts val

sweep :: (MonadIO m, Ord k) => k -> Handler k v -> Int -> TMap k v -> m ()
sweep key f n tmap = liftIO $ async (delay n) >>= link
  where
    delay x = do
        liftIO $ threadDelay x
        e <- M.lookup key `atomicRead` tmap
        void $ traverse evict e
    evict (T t v) = do
        ts <- liftIO currentTime
        let y = fromIntegral $ ts - t
        if y <= 0
         then delete key tmap >> f key v ts
         else delay y
    evict _ =  return ()

delete :: (MonadIO m, Ord k) => k -> TMap k v -> m ()
delete key tmap = atomic $! do
    m <- M.delete key `liftM` readTVar tmap
    writeTVar tmap m

atomicRead :: MonadIO m => (a -> b) -> TVar a -> m b
atomicRead f tvar = f `liftM` atomic (readTVar tvar)

atomic :: MonadIO m => STM a -> m a
atomic = liftIO . atomically
