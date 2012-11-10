-- |
-- Module      : Numbers.Whisper
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Whisper where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time.Clock.POSIX
import Numbers.Map

type Time  = Time Integer
type Value = Value Double

data Point = Point Time Value

data Whisper k = Whisper (TMap k Point)

newWhisper :: DB

currentTime :: IO Time
currentTime = truncate `liftM` getPOSIXTime


-- Check how statsd serializes various things to graphite and implement that
-- first, before storing a similar format in time series here, and provide a
-- --resolution cmdarg to specify number of samples to store, and what to average on max
-- expire samples for keys individualy


