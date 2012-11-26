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

module Numbers.Log where

import Control.Monad
import Numbers.Types
import System.Log.FastLogger
import System.IO
import System.IO.Unsafe

infoL :: Loggable a => a -> IO ()
infoL = errorL

errorL :: Loggable a => a -> IO ()
errorL = logL defaultLogger

logL :: Loggable a => Logger -> a -> IO ()
logL logger s = loggerPutBuilder logger $ s &&> "\n"

defaultLogger :: Logger
defaultLogger = unsafePerformIO $ mkLogger True stdout
{-# NOINLINE defaultLogger #-}

newLogger :: Loggable a => FilePath -> IO (a -> IO ())
newLogger path = do
    h <- case path of
             "stdout" -> return stdout
             "stderr" -> return stderr
             _        -> openFile path AppendMode
    logL `liftM` mkLogger True h
