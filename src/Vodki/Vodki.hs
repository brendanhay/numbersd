-- |
-- Module      : Vodki.Vodki
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Vodki.Vodki (
    -- * Monad
      Vodki

    -- * Functions
    , runVodki
    , store
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Vodki.Sink
import Vodki.Metric
import Vodki.Store

import qualified Data.ByteString.Char8 as BS

class (Monad m, MonadIO m, MonadPlus m) => MonadVodki m where
    liftVodki :: Vodki a -> m a

data VodkiState = VodkiState
    { _received :: BS.ByteString -> IO ()
    , _counters :: Store Counter
    , _timers   :: Store Timer
    , _gauges   :: Store Gauge
    , _sets     :: Store Set
    }

newtype Vodki a = Vodki
    { unVodki :: ReaderT VodkiState IO (Maybe a)
    }

instance Monad Vodki where
    return          = Vodki . return . Just
    fail _          = Vodki $ return Nothing
    (Vodki m) >>= f = Vodki $ do
        r <- m
        case r of
            Just v  -> unVodki $! f v
            Nothing -> return Nothing

instance MonadIO Vodki where
    liftIO m = Vodki $! liftM Just $! liftIO m

instance MonadPlus Vodki where
    mzero       = Vodki $ return Nothing
    a `mplus` b = Vodki $ do
        r <- unVodki a
        case r of
            Just _  -> return r
            Nothing -> unVodki b

instance MonadVodki Vodki where
    liftVodki = id

runVodki :: [Sink] -> Int -> [Sink] -> Vodki a -> IO ()
runVodki pre secs post (Vodki m) = do
    ss <- VodkiState
          (emitAll pre)
          <$> f (g post)
          <*> f (g post)
          <*> f (g post)
          <*> f (g post)
    _  <- runReaderT m ss
    return ()
 where
   f = newStore secs
   g sinks k v ts n = mapM_ (flip emit $ encode k v ts n) sinks

store :: MonadVodki m => BS.ByteString -> m ()
store bstr = forM_ (split bstr) bucket
  where
    split = filter (not . BS.null) . BS.lines

bucket :: MonadVodki m => BS.ByteString -> m ()
bucket bstr = msum [f _counters, f _timers, f _gauges, f _sets]
  where
    f g = case decode bstr of
        Just (k, m) -> do
            h <- state _received
            s <- state g
            liftIO $ do
                h bstr
                insert s k m
        Nothing ->
            fail "Failed to parse"

state :: MonadVodki m => (VodkiState -> a) -> m a
state f = liftVodki $ f `liftM` (Vodki $ liftM Just ask)
