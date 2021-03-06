{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Numbers.Config
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Config (
    -- * Exported Types
      Config(..)

    -- * Functions
    , parseConfig
    ) where

import Control.Monad
import Data.Aeson
import Data.Lens.Common
import Data.Lens.Template
import Data.List                       (intersect)
import Data.List.Split                 (splitOn)
import Data.Monoid                     (mempty, mconcat)
import Data.Version                    (showVersion)
import Numbers.Log
import Numbers.Types
import Numbers.Whisper.Series          (maxResolution)
import Paths_numbersd                  (version)
import System.Console.CmdArgs.Explicit
import System.Environment
import System.Exit

import qualified Data.ByteString.Char8 as BS

data Config = Help | Version | Config
    { _listeners    :: [Uri]
    , _httpPort     :: Maybe Int
    , _buffer       :: Int
    , _resolution   :: Int
    , _interval     :: Int
    , _percentiles  :: [Int]
    , _logEvents    :: [String]
    , _prefix       :: String
    , _graphites    :: [Uri]
    , _broadcasts   :: [Uri]
    , _downstreams  :: [Uri]
    }

$(makeLens ''Config)

instance Loggable Config where
    build Config{..} = mconcat
        [ sbuild "Configuration:"
        , "\n -> Listeners:      " <&& _listeners
        , "\n -> HTTP Port:      " <&& _httpPort
        , "\n -> Buffer Size:    " <&& _buffer
        , "\n -> Resolution:     " <&& _resolution
        , "\n -> Flush Interval: " <&& _interval
        , "\n -> Percentiles:    " <&& _percentiles
        , "\n -> Log Events:     " <&& _logEvents
        , "\n -> Prefix:         " <&& _prefix
        , "\n -> Graphites:      " <&& _graphites
        , "\n -> Broadcasts:     " <&& _broadcasts
        , "\n -> Downstreams:    " <&& _downstreams
        ]
    build _ = mempty

instance ToJSON Config where
    toJSON Config{..} = object
        [ "listeners"   .= _listeners
        , "http_port"   .= _httpPort
        , "buffer_size" .= _buffer
        , "resolution"  .= _resolution
        , "interval"    .= _interval
        , "percentiles" .= _percentiles
        , "log_events"  .= _logEvents
        , "prefix"      .= _prefix
        , "graphites"   .= _graphites
        , "broadcasts"  .= _broadcasts
        , "downstreams" .= _downstreams
        ]
    toJSON _ = Null

defaultConfig :: Config
defaultConfig = Config
    { _listeners    = [Udp (BS.pack "0.0.0.0") 8125]
    , _httpPort     = Nothing
    , _buffer       = 8
    , _interval     = 10
    , _resolution   = 60
    , _percentiles  = [90]
    , _logEvents    = []
    , _graphites    = []
    , _prefix       = "stats"
    , _broadcasts   = []
    , _downstreams  = []
    }

parseConfig :: IO Config
parseConfig = do
    a <- getArgs
    n <- getProgName
    case processValue (flags n) a of
        Help    -> print (helpText [] HelpFormatOne $ flags n) >> exitSuccess
        Version -> putStrLn (info n) >> exitSuccess
        c       -> validate c >> infoL c >> return c

info :: String -> String
info name = concat
    [ name
    , " version "
    , showVersion version
    , " (C) Brendan Hay <brendan.g.hay@gmail.com> 2012"
    ]

validate :: Config -> IO ()
validate Config{..} = do
    check (null _listeners)
       "--listeners cannot be blank"
    check (not . null $ _listeners `intersect` sinks)
        "--listeners cannot contain any URI used by --{graphites,broadcasts,downstreams}"
    check (not . null $ _listeners `intersect` ["file://stdout", "file://stderr"])
        "--listeners cannot read from stdout or stderr"
    check ("file://stdin" `elem` sinks)
        "--{graphites,broadcasts,downstreams} cannot cannot write to stdin"
    check (_buffer < 1)
        "--buffer must be greater than 0"
    check (_interval < 1)
        "--interval must be greater than 0"
    check (_interval >= _resolution)
        "--resolution must be greater than --interval"
    check (_resolution > maxResolution) $
        "--resolution must be less than " ++ show maxResolution
    check (null _percentiles)
        "--percentiles cannot be blank"
    return ()
  where
    check p m = when p $ putStrLn m >> exitWith (ExitFailure 1)
    sinks     = _graphites ++ _broadcasts ++ _downstreams
validate _ = return ()

flags :: String -> Mode Config
flags name = mode name defaultConfig "Numbers"
    (flagArg (\x _ -> Left $ "Unexpected argument " ++ x) "")
    [ flagReq ["listeners"] (many listeners) "[URI]"
      "Incoming stats address and port combinations"

    , flagReq ["http"] (parse (setL httpPort . Just . read)) "PORT"
      "HTTP port to serve the overview and time series on"

    , flagReq ["buffer"] (one buffer) "INT"
      "Number of packets to buffer, from all listeners"

    , flagReq ["resolution"] (one resolution) "INT"
      "Resolution in seconds for time series data"

    , flagReq ["interval"] (one interval) "INT"
      "Interval in seconds between key flushes to subscribed sinks"

    , flagReq ["percentiles"] (many percentiles) "[INT]"
      "Calculate the Nth percentile(s) for timers"

    , flagReq ["events"] (parse (setL logEvents . splitOn ",")) "[EVENT]"
      "Log [receive,invalid,parse,flush] events"

    , flagReq ["prefix"] (many prefix) "STR"
      "Prepended to keys in the http interfaces and graphite"

    , flagReq ["graphites"] (many graphites) "[URI]"
      "Graphite hosts to deliver metrics to"

    , flagReq ["broadcasts"] (many broadcasts) "[URI]"
      "Hosts to broadcast raw unaggregated packets to"

    , flagReq ["downstreams"] (many downstreams) "[URI]"
      "Hosts to forward aggregated counters to"

    , flagNone ["help", "h"] (\_ -> Help)
      "Display this help message"

    , flagVersion $ \_ -> Version
    ]
  where
    one  l    = parse (setL l . read)
    many l    = parse (setL l . map read . splitOn ",")
    parse f s = Right . f s
