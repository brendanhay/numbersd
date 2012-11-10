-- |
-- Module      : Numbers.Store
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Store (
    -- * Opaque
      Store
    , newStore

    -- * Functions
    , insert
    ) where

import Control.Applicative    hiding (empty)
import Control.Monad
import Control.Concurrent
import Data.Time.Clock.POSIX
import Numbers.Sink
import Numbers.TMap
import Numbers.Types

import qualified Data.ByteString.Char8 as BS

data Store = Store
    { interval :: Int
    , sinks    :: [Sink]
    , store    :: TMap Key Metric
    }

newStore :: Int -> [Sink] -> IO Store
newStore n sinks = Store n sinks <$> newTMap

insert :: Store -> BS.ByteString -> IO ()
insert s@Store{..} bstr = do
    emit sinks $ Receive bstr
    forM_ (filter (not . BS.null) $ BS.lines bstr) f
  where
    f b = case decode metric b of
        Just (k, v) -> bucket s k v
        Nothing     -> emit sinks $ Invalid bstr

bucket :: Store -> Key -> Metric -> IO ()
bucket s@Store{..} key val = updateTMap store key f
  where
    f (Just x) = return $ x `aggregate` val
    f Nothing  = flush s key >> return val

flush :: Store -> Key -> IO ()
flush Store{..} key = void . forkIO $ do
    threadDelay n
    v  <- deleteTMap store key
    ts <- getPOSIXTime
    emit sinks $ Flush key v ts interval
  where
    n = interval * 1000000
