-- |
-- Module      : Vodki.Sink.Console
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Vodki.Sink.Console (
      EventName
    , consoleSink
    ) where

import Data.String
import Vodki.Sink.Internal

import qualified Data.ByteString.Char8 as BS

newtype EventName = EventName String deriving (Eq)

instance Read EventName where
    readsPrec _ a = do
        (h, b) <- lex a
        return (EventName h, b)

instance Show EventName where
    show (EventName s) = s

instance IsString EventName where
    fromString = EventName

consoleSink :: [EventName] -> IO Sink
consoleSink evts = runSink $ (flip $ foldl f) handlers
  where
    f s (k, g) = if k `elem` evts then g s else s

handlers :: [(EventName, Sink -> Sink)]
handlers =
    [ ("receive", setReceive $ \v -> putStrLn $ "Receive: " ++ BS.unpack v)
    , ("invalid", setInvalid $ \v -> putStrLn $ "Invalid: " ++ BS.unpack v)
    , ("parse", setParse $ \k v -> putStrLn $ "Parse: " ++ show k ++ " " ++ show v)
    , ("flush", setFlush $ \k v _ _ -> putStrLn $ "Flush: " ++ show k ++ " " ++ show v)
    ]
