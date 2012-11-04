{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Vodki.Sink.Repeater
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Vodki.Sink.Repeater (
      repeater
    ) where

import Control.Applicative        hiding (empty)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Setters
import Data.Time.Clock.POSIX
import Vodki.Sink.Internal

import qualified Data.ByteString.Char8 as BS

data Config = Config { host :: String, port :: Int }

repeater :: [Config] -> IO Sink
repeater cs = runSink . setReceive $ \s ->
    putStrLn $ "Repeat: " ++ BS.unpack s
