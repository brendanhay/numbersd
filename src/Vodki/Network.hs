-- |
-- Module      : Vodki.Network
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Vodki.Network (
    -- * Functions
      openSocket

    -- * Re-exported
    , Socket
    , SocketType(..)
    , S.recv
    , bindSocket
    , connect
    , setSocketOption
    ) where

import Network.Socket

import qualified Network.Socket.ByteString as S

openSocket :: Maybe String -> Int -> SocketType -> IO (Socket, SockAddr)
openSocket host port stype = do
    i:_ <- getAddrInfo proto host (Just $ show port)
    s   <- socket (addrFamily i) stype defaultProtocol
    return (s, addrAddress i)
  where
    proto = case stype of
        Datagram -> Just $ defaultHints { addrFlags = [AI_PASSIVE] }
        _        -> Nothing
