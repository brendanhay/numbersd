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
      Options(..)

    -- * Functions
    , parseOptions
    ) where

import Data.Setters
import Data.List.Split                 (splitOn)
import Data.Version                    (showVersion)
import Paths_vodki                     (version)
import System.Console.CmdArgs.Explicit
import System.Environment
import System.Exit
import Numbers.Log
import Numbers.Sink
import Numbers.Socket

data Options = Help | Version | Options
    { listener       :: Addr
    , status         :: Addr
    , interval       :: Int
    , percentile     :: [Int]
    , logEvents      :: [EventName]
    , logPath        :: String
    , graphite       :: [Addr]
    , graphitePrefix :: String
    , broadcast      :: [Addr]
    , downstream     :: [Addr]
    }

$(declareSetters ''Options)

instance Show Options where
    show Options{..} = unlines
        [ "Configuration: "
        , " -> UDP Listener:    " ++ show listener
        , " -> HTTP Status:     " ++ show status
        , " -> Flush Interval:  " ++ show interval
        , " -> Percentile:      " ++ show percentile
        , " -> Log Events:      " ++ show logEvents
        , " -> Log Path:        " ++ show logPath
        , " -> Graphite:        " ++ show graphite
        , " -> Graphite Prefix: " ++ show graphitePrefix
        , " -> Broadcast:       " ++ show broadcast
        , " -> Downstream:      " ++ show downstream
        ]
    show _ = ""

defaultOptions :: Options
defaultOptions = Options
    { listener       = Addr "0.0.0.0" 8125
    , status         = Addr "0.0.0.0" 8126
    , interval       = 10
    , percentile     = [90]
    , logEvents      = []
    , logPath        = "stdout"
    , graphite       = []
    , graphitePrefix = "stats"
    , broadcast      = []
    , downstream     = []
    }

parseOptions :: IO Options
parseOptions = do
    a <- getArgs
    n <- getProgName
    case processValue (flags n) a of
        Help    -> print (helpText [] HelpFormatOne $ flags n) >> exitSuccess
        Version -> print (info n) >> exitSuccess
        opts    -> infoL (show opts) >> return opts

info :: String -> String
info name = concat
    [ name
    , " version "
    , showVersion version
    , " (C) Brendan Hay <brendan@soundcloud.com> 2012"
    ]

flags :: String -> Mode Options
flags name = mode name defaultOptions "Numbers"
    (flagArg (\x _ -> Left $ "Unexpected argument " ++ x) "")
    [ flagReq ["listen"] (f setListener) "ADDR:PORT"
      "Incoming stats UDP address and port"

    , flagReq ["status"] (f setStatus) "ADDR:PORT"
      "HTTP status page address and port"

    , flagReq ["interval"] (f setInterval) "INT"
      "Interval between key flushes to subscribed sinks"

    , flagReq ["percentile"] (g setPercentile) "[INT]"
      "Calculate the Nth percentile(s) for timers"

    , flagReq ["log"] (g setLogEvents) "[EVENT]"
      "Log [receive,invalid,parse,flush] events"

    , flagReq ["log-path"] (\s o -> Right $ setLogPath s o) "PATH"
      "Log file path, or stdout"

    , flagReq ["graphite"] (g setGraphite) "[ADDR:PORT]"
      "Graphite hosts to deliver metrics to"

    , flagReq ["graphite-prefix"] (g setGraphitePrefix) "STRING"
      "Prepended to all keys flushed to graphite"

    , flagReq ["broadcast"] (g setBroadcast) "[ADDR:PORT]"
      "Hosts to broadcast raw unaggregated packets to"

    , flagReq ["downstream"] (g setDownstream) "[ADDR:PORT]"
      "Hosts to forward aggregated counters to"

    , flagNone ["help", "h"] (\_ -> Help)
      "Display this help message"

    , flagVersion $ \_ -> Version
    ]
  where
    f upd s = Right . upd (read s)
    g upd s = Right . upd (map read $ splitOn "," s)
