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
    -- * Exported Types
      Policy(..)

    -- * Opaque
    , Map
    , empty

    -- * Functions
    , toList
    , keys
    , lookup
    , update
    ) where

import Prelude                  hiding (lookup)
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async hiding (wait)
import Numbers.Types            hiding (P)

import qualified Numbers.Map.Internal as I


type Handler k v = k -> v -> Time -> IO ()

data Policy k v = Reset Int (Handler k v)
                | Continue Int (Handler k v)
                | NoPolicy

data Map k v = Map
    { _policy :: Policy k v
    , _imap   :: I.Map k v
    }

empty :: Policy k v -> MonadIO m => m (Map k v)
empty p = Map p `liftM` I.empty

toList :: MonadIO m => Map k v -> m [(k, v)]
toList = I.toList . _imap

keys :: MonadIO m => Map k v -> m [k]
keys = I.keys . _imap

lookup :: (MonadIO m, Ord k) => k -> Map k v -> m (Maybe v)
lookup key = I.lookup key . _imap

update :: (MonadIO m, Ord k) => k -> (Maybe v -> v) -> Map k v -> m ()
update key f Map{..} = do
  u <- I.update key f _imap
  case u of
    I.New -> scheduleSweep key _policy _imap
    I.Existing -> return ()

scheduleSweep :: (MonadIO m, Ord k) => k -> Policy k v -> I.Map k v -> m ()
scheduleSweep _ NoPolicy _ = return ()
scheduleSweep key (Reset d h) imap = sweep d h I.modify key imap
scheduleSweep key (Continue d h) imap = sweep d h I.create key imap

sweep :: (MonadIO m, Ord k) => Int -> Handler k v -> (I.Entry v -> Time) -> k -> I.Map k v -> m ()
sweep delay handle lastTime key imap = do
  liftIO $ async waitAndCheck >>= link
  where
    waitAndCheck = do
      _ <- liftIO . threadDelay $ delay * 1000000
      now <- liftIO currentTime
      me <- I.deleteIf (\e -> lastTime e + fromIntegral delay <= now) key imap
      maybe waitAndCheck (\e -> handle key (I.value e) now) me
