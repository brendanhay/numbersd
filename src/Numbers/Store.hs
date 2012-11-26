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
    , runStore
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Conduit             hiding (Flush)
import Numbers.Conduit.Internal
import Numbers.Types

import qualified Control.Concurrent.STM.Map as M
import qualified Data.ByteString.Char8      as BS

data Store = Store
    { _percentiles :: [Int]
    , _interval    :: Int
    , _sinks       :: [EventSink]
    , _tmap        :: M.Map Key Metric
    }

runStore :: [Int] -> Int -> [EventSink] -> TBQueue BS.ByteString -> IO ()
runStore qs n hs q = runResourceT $ sourceQueue q $$ sink
  where
    sink = bracketP
        (liftIO $ newStore qs n hs)
        (\_ -> return ())
        (\s -> awaitForever $ liftIO . flip insert s)

newStore :: [Int] -> Int -> [EventSink] -> IO Store
newStore qs n sinks = Store qs n sinks `fmap` M.empty

insert :: BS.ByteString -> Store -> IO ()
insert bstr s@Store{..} = do
    pushEvent _sinks $ Receive bstr
    forM_ (filter (not . BS.null) $ BS.lines bstr) f
  where
    f b = case decode metricParser b of
        Just (k, v) -> bucket k v s
        Nothing     -> pushEvent _sinks $ Invalid bstr

bucket :: Key -> Metric -> Store -> IO ()
bucket key val s@Store{..} = M.update key f _tmap
  where
    f (Just x) = return $ x `aggregate` val
    f Nothing  = flush key s >>= link >> return val

flush :: Key -> Store -> IO (Async ())
flush key Store{..} = async $ do
    threadDelay $ _interval * 1000000
    M.delete key _tmap >>= f
  where
    f Nothing  = return ()
    f (Just v) = do
        ts <- currentTime
        mapM_ (pushEvent _sinks . Flush ts)
            $ calculate _percentiles _interval key v
