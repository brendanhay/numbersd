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
    , openSocket
    , Socket
    , SocketType(..)
    , S.send
    , S.recv
    , bindSocket
    , connect
    , setSocketOption

    -- * Retryable Sockets
    , SocketR
    , openSocketR
    , sendR
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
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
    , connRef  :: IORef Socket
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
    (s, _) <- openSocket addr typ
    ref    <- newIORef s
    return $ SocketR addr typ ref

sendR :: SocketR -> BS.ByteString -> IO ()
sendR SocketR{..} bstr = retry 2
  where
    retry 0 = fail "Out of retries, bitches!"
    retry n = do
        s <- readIORef connRef
        liftIO $ S.sendAll s bstr

 -- `catches`
 --             [ Handler (\e -> throw (e :: AsyncException))
 --             , Handler (\e -> do
 --                            liftIO . putStrLn $ "Send failed: " ++ show e ++ show n ++ " more tries left."
 --                            reopenR
 --                            retry $ n - 1)
 --             ]

-- reopenR :: SocketR -> IO ()
-- reopenR r@SocketR{..} = do
--     readIORef connRef >>= close
--     (s, _) <- openSocket connAddress connType
--     writeIORef connRef s
--     return r
