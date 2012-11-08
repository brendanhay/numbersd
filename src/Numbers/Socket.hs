-- |
-- Module      : Numbers.Socket
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Socket (
    -- * Socket Wrapper
      Socket
    , tcp
    , listen
    , accept
    , connect
    , send
    , recv

    -- * Re-exports
    , withSocketsDo
    ) where

import Control.Monad         (when)
import Network.Socket hiding (Socket, listen, accept, connect, send, recv)
import Numbers.Types

import qualified Data.ByteString.Char8     as BS
import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as SS

data Socket = Socket
    { _uri  :: Uri
    , _sock :: S.Socket
    , _addr :: SockAddr
    }

tcp :: Uri -> Bool
tcp (Tcp _ _) = True
tcp _         = False

listen :: Uri -> IO Socket
listen uri = do
    s@Socket{..} <- open uri
    setSocketOption _sock ReuseAddr 1
    bind _sock _addr
    when (tcp uri) (S.listen _sock maxListenQueue)
    return s

accept :: Socket -> IO Socket
accept Socket{..} = do
    (s, a) <- S.accept _sock
    return $ Socket _uri s a

connect :: Uri -> IO Socket
connect uri = do
    s@Socket{..} <- open uri
    when (tcp uri) (setSocketOption _sock KeepAlive 1)
    return s

send :: Socket -> BS.ByteString -> IO ()
send Socket{..} bstr = SS.sendAllTo _sock bstr _addr

recv :: Socket -> IO BS.ByteString
recv = flip SS.recv 2048 . _sock

open :: Uri -> IO Socket
open uri = do
    i:_ <- getAddrInfo (Just defaultHints) (Just h) (Just p)
    s   <- socket (addrFamily i) t defaultProtocol
    return . Socket uri s $ addrAddress i
  where
    h = BS.unpack $ _host uri
    p = show $ _port uri
    t = if tcp uri then Stream else Datagram
