-- |
-- Module      : Vodki.Sink
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Vodki.Sink (
      Event(..)

    -- * Opaque
    , Sink
    , emit
    , debugSink
    ) where

import Control.Applicative        hiding (empty)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time.Clock.POSIX
import Vodki.Metric

import qualified Data.ByteString.Char8 as BS

data Event = Receive BS.ByteString
           | Invalid BS.ByteString
           | Parse Key Metric
           | Flush Key Metric POSIXTime Int

data Sink = Sink
    { receive :: BS.ByteString -> IO ()
    , invalid :: BS.ByteString -> IO ()
    , parse   :: Key -> Metric -> IO ()
    , flush   :: Key -> Metric -> POSIXTime -> Int -> IO ()
    , events  :: TQueue Event
    }

newSink :: IO Sink
newSink = Sink f f (\_ _ -> return ()) (\_ _ _ _ -> return ())
    <$> atomically newTQueue
  where
    f _ = return ()

runSink :: Sink -> IO Sink
runSink s@Sink{..} = do
    _ <- forkIO . forever $ do
        e <- atomically $ readTQueue events
        case e of
            (Receive bs)     -> receive bs
            (Invalid bs)     -> invalid bs
            (Parse k v)      -> parse k v
            (Flush k v ts n) -> flush k v ts n
    return s

emit :: [Sink] -> Event -> IO ()
emit sinks evt = forM_ sinks (\s -> atomically $ writeTQueue (events s) evt)

debugSink :: IO Sink
debugSink = do
    s <- newSink
    runSink $ s { parse = parse' }
  where
    parse' k v = putStrLn $ "Debug: " ++ show k ++ " " ++ show v
