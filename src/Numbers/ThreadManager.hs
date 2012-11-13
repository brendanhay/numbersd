-- |
-- Module      : Numbers.ThreadManager
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.ThreadManager (
      ThreadManager
    , ThreadStatus(..)
    , newManager
    , fork
    , threadStatus
    , wait
    , waitAll
    ) where

import Control.Concurrent      (ThreadId, forkIO)
import Control.Concurrent.MVar
import Control.Exception       (SomeException, try)
import Control.Monad           (when)

import qualified Numbers.TMap as M

data ThreadStatus = Running
                  | Finished
                  | Threw SomeException
                    deriving Show

newtype ThreadManager = TM (M.TMap ThreadId (MVar ThreadStatus))

newManager :: IO ThreadManager
newManager = TM `fmap` M.empty

fork :: ThreadManager -> IO () -> IO ThreadId
fork (TM tmap) io = do
    var <- newEmptyMVar
    tid <- forkIO $ try io >>= putMVar var . either Threw (const Finished)
    M.insert tid var tmap
    return tid

threadStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
threadStatus (TM tmap) tid = do
    v <- M.lookup tid tmap
    case v of
       Just x  -> f x
       Nothing -> return Nothing
  where
    f x = do
        s <- tryTakeMVar x
        case s of
            Just y  -> M.delete tid tmap >> return (Just y)
            Nothing -> return $ Just Running

wait :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
wait (TM tmap) tid = do
    ss <- M.delete tid tmap
    case ss of
        Just x  -> Just `fmap` takeMVar x
        Nothing -> return Nothing

waitAll :: ThreadManager -> IO ()
waitAll m@(TM tmap) = do
    ts <- M.keys tmap
    ss <- mapM (threadStatus m) ts
    mapM_ (wait m) ts
    when (foldr f False ss) $ waitAll m
  where
    f _ True               = True
    f (Just Running) False = True
    f _ False              = False