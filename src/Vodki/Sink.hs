{-# LANGUAGE ExistentialQuantification #-}

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
    -- * Opaque
      Sink
    , Console
    , Graphite

    -- * Functions
    , emit
    , emitAll

    -- * Sink Constructors
    , sinkStdout
    , sinkGraphite
    ) where

import Control.Monad
import Network.Socket hiding (connect)
import Vodki.Network

import qualified Data.ByteString.Char8 as BS
import qualified Network.Socket        as S

class Emit a where
    emit :: a -> BS.ByteString -> IO ()

data Sink = forall a. Emit a => Sink a

emitAll :: [Sink] -> BS.ByteString -> IO ()
emitAll sinks bstr = mapM_ (`emit` bstr) sinks

instance Emit Sink where
    emit (Sink s) = emit s

data Console = Console

sinkStdout :: Sink
sinkStdout = Sink Console

instance Emit Console where
    emit _ = BS.putStrLn . BS.append "Console: "

data Graphite = Graphite

sinkGraphite :: Sink
sinkGraphite = Sink Graphite

instance Emit Graphite where
    emit _ = BS.putStrLn . BS.append "Graphite: "

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
