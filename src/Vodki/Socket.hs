-- |
-- Module      : Vodki.Socket
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Vodki.Socket (
    -- * Exported Types
      Addr(..)

    -- * Vanilla Sockets
    , Socket
    , SocketType(..)
    , openSocket
    , bindSocket
    , S.recv

    -- * Retryable Sockets
    , SocketR
    , openSocketR
    , sendR
    ) where

import Control.Concurrent
import Control.Exception
import Data.IORef
import Network.Socket

import qualified Data.ByteString.Char8     as BS
import qualified Network.Socket.ByteString as S

data Addr = Addr String Int

instance Read Addr where
    readsPrec _ a = do
        (h, b)   <- lex a
        (":", c) <- lex b
        (p, d)   <- lex c
        return (Addr h $ read p, d)

instance Show Addr where
    show (Addr h p) = h ++ ":" ++ show p

data SocketR = SocketR
    { connAddr :: Addr
    , connType :: SocketType
    , connRef  :: IORef (Socket, SockAddr)
    }

openSocket :: Addr -> SocketType -> IO (Socket, SockAddr)
openSocket (Addr host port) stype = do
    i:_ <- getAddrInfo proto (Just host) (Just $ show port)
    s   <- socket (addrFamily i) stype defaultProtocol
    return (s, addrAddress i)
  where
    proto = case stype of
        Datagram -> Just $ defaultHints { addrFlags = [AI_PASSIVE] }
        _        -> Nothing

openSocketR :: Addr -> SocketType -> IO SocketR
openSocketR addr typ = do
    sa <- openSocket addr typ
    r  <- newIORef sa
    return $ SocketR addr typ r

sendR :: SocketR -> BS.ByteString -> IO ()
sendR r@SocketR{..} bstr = retry 3
  where
    delay   = 3
    retry 0 = fail "Out of retries, bitches!"
    retry n = do
        (s, a) <- readIORef connRef
        S.sendAllTo s bstr a `catches`
            [ Handler (\e -> throw (e :: AsyncException))
            , Handler (\e -> do
                  msgR e n delay
                  threadDelay $ delay * 1000000
                  reopenR r
                  retry $ n - 1)
            ]

msgR :: SomeException -> Int -> Int -> IO ()
msgR e retries delay = putStrLn $ concat
    [ show e
    , " -> "
    , show retries
    , " more attempts left, trying in "
    , show delay
    , " seconds"
    ]

reopenR :: SocketR -> IO ()
reopenR SocketR{..} = do
    (s, _) <- readIORef connRef
    close s
    sa <- openSocket connAddr connType
    writeIORef connRef sa