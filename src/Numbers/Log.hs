-- |
-- Module      : Numbers.Log
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Log (
      initLogger
    , newLogger
    , infoL
    , errorL
    ) where

import System.IO
import System.Log.Formatter
import System.Log.Logger
import System.Log.Handler        (setFormatter)
import System.Log.Handler.Simple
import System.Log.Handler.Syslog

initLogger :: IO ()
initLogger = do
    removeAllHandlers
    h <- streamHandler stdout level
    configure name h

newLogger :: String -> FilePath -> IO (String -> IO ())
newLogger n path = do
    h <- if path == "stdout"
          then streamHandler stdout level
          else fileHandler path level
    configure n h
    infoM n "Creating"
    return $ infoM n

infoL  = infoM name
errorL = errorM name

name :: String
name = "Main"

configure n h = do
    updateGlobalLogger n (setLevel level)
    updateGlobalLogger n (addHandler $ setFormatter h format)

level = INFO

format = simpleLogFormatter "[$time : $loggername : $prio] $msg"