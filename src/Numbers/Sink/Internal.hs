{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Numbers.Sink.Internal
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Sink.Internal (
    -- * Event Constructors
      Event(..)

    -- * Opaque
    , Sink(..)
    , emit
    , newSink
    , runSink

    -- * Lenses
    , receive
    , invalid
    , parse
    , flush
    ) where

import Control.Applicative    hiding (empty)
import Control.Monad
import Control.Concurrent            (forkIO)
import Control.Concurrent.STM
import Data.Lens.Template
import Data.Time.Clock.POSIX
import Numbers.Types

import qualified Data.ByteString.Char8 as BS

data Event = Receive BS.ByteString
           | Invalid BS.ByteString
           | Parse Key Metric
           | Flush Key Metric POSIXTime Int

data Sink = Sink
    { _receive :: BS.ByteString -> IO ()
    , _invalid :: BS.ByteString -> IO ()
    , _parse   :: (Key, Metric) -> IO ()
    , _flush   :: (Key, Metric, POSIXTime, Int) -> IO ()
    , _events  :: TQueue Event
    }

$(makeLens ''Sink)

emit :: [Sink] -> Event -> IO ()
emit sinks evt = forM_ sinks (\s -> atomically $ writeTQueue (_events s) evt)

newSink :: (Sink -> a) -> IO a
newSink = flip liftM $ Sink f f f f <$> atomically newTQueue
  where
    f _ = return ()

runSink :: Sink -> IO ()
runSink Sink{..} = void . forkIO . forever $ do
    e <- atomically $ readTQueue _events
    case e of
        (Receive bs)     -> _receive bs
        (Invalid bs)     -> _invalid bs
        (Parse k v)      -> _parse (k, v)
        (Flush k v ts n) -> _flush (k, v, ts, n)
