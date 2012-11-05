{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Vodki.Config
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Vodki.Config (
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
import Vodki.Sink.Console
import Vodki.Socket

data Options = Help | Version | Options
    { server      :: Addr
    , management  :: Addr
    , interval    :: Int
    , percentiles :: [Int]
    , console     :: [EventName]
    , graphite    :: [Addr]
    , repeater    :: [Addr]
    , statsd      :: [Addr]
    }

$(declareSetters ''Options)

instance Show Options where
    show Options{..} = unlines
        [ "Configuration: "
        , " -> Server:         " ++ show server
        , " -> Management:     " ++ show management
        , " -> Flush Interval: " ++ show interval
        , " -> Percentiles:    " ++ show percentiles
        , " -> Console:        " ++ show console
        , " -> Graphite:       " ++ show graphite
        , " -> Repeater:       " ++ show repeater
        , " -> Statsd:         " ++ show statsd
        ]
    show _ = ""

defaultOptions :: Options
defaultOptions = Options
    { server      = Addr "0.0.0.0" 8125
    , management  = Addr "0.0.0.0" 8126
    , interval    = 10
    , percentiles = [90]
    , console     = []
    , graphite    = []
    , repeater    = []
    , statsd      = []
    }

parseOptions :: IO Options
parseOptions = do
    a <- getArgs
    n <- getProgName
    case processValue (flags n) a of
        Help    -> print (helpText [] HelpFormatOne $ flags n) >> exitSuccess
        Version -> print (info n) >> exitSuccess
        opts    -> putStr (show opts) >> return opts

flags :: String -> Mode Options
flags name = mode name defaultOptions "Vodki"
    (flagArg (\x _ -> Left $ "Unexpected argument " ++ x) "")
    [ flagReq ["server"] (f setServer) "ADDR:PORT" "Incoming stats UDP address and port"
    , flagReq ["management"] (f setManagement) "ADDR:PORT" "Management interface TCP address and port"
    , flagReq ["interval"] (f setInterval) "SECONDS" "Interval between key flushes to subscribed sinks"
    , flagReq ["percentiles"] (g setPercentiles) "[INT]" "Calculate the Nth percentile(s) for timers"
    , flagReq ["console"] (g setConsole) "[EVENT]" "Which [receive,invalid,parse,flush] events to log"
    , flagReq ["graphite"] (g setGraphite) "[ADDR:PORT]" "Graphite hosts to deliver metrics to"
    , flagReq ["repeater"] (g setRepeater) "[ADDR:PORT]" "Statsd hosts to forward raw unaggregated packets to"
    , flagReq ["statsd"] (g setStatsd) "[ADDR:PORT]" "Statsd hosts to forward aggregated counters to"
    , flagNone ["help", "h"] (\_ -> Help) "Display this help message"
    , flagVersion $ \_ -> Version
    ]
  where
    f upd s = Right . upd (read s)
    g upd s = Right . upd (map read $ splitOn "," s)

info :: String -> String
info name = concat
    [ name
    , " version "
    , showVersion version
    , " (C) Brendan Hay <brendan@soundcloud.com> 2012"
    ]
