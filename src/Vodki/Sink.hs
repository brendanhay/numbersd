{-# LANGUAGE ExistentialQuantification, FunctionalDependencies #-}

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

module Vodki.Sink where -- (
    -- -- * Existential Type and Constructor Wrappers
    --   Aggr
    -- , Raw

    -- -- * Opaque
    -- , Console
    -- , Graphite

    -- -- * Functions
    -- , aggr
    -- , raw

    -- -- * Sink Constructors
    -- , consoleRaw
    -- , consoleAggr
    -- , graphiteAggr
    -- ) where

import Control.Monad
import Data.Time.Clock.POSIX

import qualified Control.Concurrent.Chan.Split as C
import qualified Data.ByteString.Char8         as BS

-- class AggrSink a where
--     aggr :: (Show b, Metric b) => a -> Flush b -> IO ()

-- data Aggr = forall a. AggrSink a => Aggr a

-- instance AggrSink Aggr where
--     aggr (Aggr s) = aggr s

-- class RawSink a where
--     raw  :: a -> BS.ByteString -> IO ()

-- data Raw = forall a. RawSink a => Raw a

-- instance RawSink Raw where
--     raw (Raw s) = raw s

-- data Console = Console

-- instance AggrSink Console where
--     aggr _ Flush{..} = putStrLn $ "Console: " ++ show _value

-- instance RawSink Console where
--     raw _  = BS.putStrLn . BS.append "Console: "

-- consoleRaw :: Raw
-- consoleRaw = Raw Console

-- consoleAggr :: Aggr
-- consoleAggr = Aggr Console

-- data Graphite = Graphite

-- instance AggrSink Graphite where
--     aggr _ Flush{..} = putStrLn $ "Graphite: " ++ show _value

-- graphiteAggr :: Aggr
-- graphiteAggr = Aggr Graphite

-- tcpHandle :: String -> Int -> IO Handle
-- tcpHandle host port = do
--     s <- client (Just host) port Stream
--     setSocketOption s KeepAlive 1
--     socketToHandle s WriteMode

-- data Graphite = Graphite Handle

-- instance Connection Graphite where
--     create host port = Graphite `liftM` tcpHandle host port
--     flush _ = BS.putStrLn

-- import qualified Data.ByteString.Char8 as BS
-- import qualified Network.Socket        as S

-- class Connection a where
--     create :: String -> Int -> IO a
--     flush  :: a -> BS.ByteString -> IO ()

-- class (Connection a, Metric a) => Sink a k v where
--     encode :: k -> v -> POSIXTime -> Int -> BS.ByteString

-- -- data AnySink = forall a. Sink a => AnySink a

-- -- wrap :: Sink a => a -> AnySink
-- -- wrap = AnySink

-- test = do
--     ts <- getPOSIXTime
--     g  <- create "127.0.0.1" 9999 :: IO Graphite

--     let c = empty :: Counter

--     flush g $ encode (Key "key") c ts 10

-- data Graphite = Graphite Handle

-- instance Connection Graphite where
--     -- create h p = tcpHandle h p >>= return . Graphite
--     create host port = Graphite `liftM` tcpHandle host port

-- -- data Repeater = Repeater Handle

-- -- instance Sink Repeater where
-- --     connect = (liftM Repeater) tcpHandle
