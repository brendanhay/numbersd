{-# LANGUAGE FunctionalDependencies #-}

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
    -- * ReaderT
      Vodki
    , runVodki
    , attachSink
    , storeMetric

    , Debug(..)
    , Repeater(..)
    , Graphite(..)
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import Data.Time.Clock.POSIX
import Vodki.Types

import qualified Control.Concurrent.Chan.Split as C
import qualified Data.ByteString.Char8         as BS
import qualified Data.Map                      as M

class Sink a where
    emit :: a -> Event -> IO ()

data Debug = Debug

instance Sink Debug where
    emit _ (Bucket k v) = putStrLn $ "Debug: " ++ show k ++ " " ++ show v
    emit _ _ = return ()

data Repeater = Repeater

instance Sink Repeater where
    emit _ (Insert s) = putStrLn $ "Repeater: " ++ BS.unpack s
    emit _ _ = return ()

data Graphite = Graphite

instance Sink Graphite where
    emit _ (Flush k v ts n) = putStrLn $ "Graphite: " ++ show k ++ " " ++ show v ++ " " ++ show ts
    emit _ _ = return ()

data Store = Store
    { delay  :: Int
    , events :: C.SendPort Event
    , tvar   :: TVar (M.Map Key (TVar Metric))
    }

type Vodki a = ReaderT Store IO a

runVodki :: Int -> Vodki a -> IO a
runVodki n vodki = do
    s <- Store n <$> C.newSendPort <*> atomically (newTVar M.empty)
    runReaderT vodki s

attachSink :: Sink a => a -> Vodki ()
attachSink sink = do
    Store{..} <- ask
    source <- liftIO $ C.listen events
    liftIO . void . forkIO . forever $ C.receive source >>= emit sink

storeMetric :: BS.ByteString -> Vodki ()
storeMetric bstr = ask >>= liftIO . flip insert bstr

insert :: Store -> BS.ByteString -> IO ()
insert s@Store{..} bstr = do
    C.send events $ Insert bstr
    forM_ (filter (not . BS.null) $ BS.lines bstr) parse
  where
    parse b = case decode b of
        Just (k, v) -> bucket s k v
        Nothing     -> C.send events $ Invalid b

bucket :: Store -> Key -> Metric -> IO ()
bucket s@Store{..} key val = do
    C.send events $ Bucket key val
    m <- readTVarIO tvar
    case M.lookup key m of
        Just v  -> atomically $ modifyTVar' v (append val)
        Nothing -> do
            atomically $ do
                v <- newTVar val
                writeTVar tvar $! M.insert key v m
            flush s key

flush :: Store -> Key -> IO ()
flush s@Store{..} key = void . forkIO $ do
    threadDelay n
    v  <- delete s key
    ts <- getPOSIXTime
    C.send events $ Flush key v ts delay
  where
    n = delay * 1000000

delete :: Store -> Key -> IO Metric
delete Store{..} key = atomically $ do
    m <- readTVar tvar
    writeTVar tvar $! M.delete key m
    readTVar (fromJust $ M.lookup key m)