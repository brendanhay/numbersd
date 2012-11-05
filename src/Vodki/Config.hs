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
import Vodki.Sink
import Vodki.Socket

data Options = Help | Version | Options
    { server      :: Addr
    , management  :: Addr
    , interval    :: Int
    , percentiles :: [Int]
    , log         :: [EventName]
    , logPath     :: FilePath
    , graphite    :: [Addr]
    , broadcast   :: [Addr]
    , upstream    :: [Addr]
    }

$(declareSetters ''Options)

instance Show Options where
    show Options{..} = unlines
        [ "Configuration: "
        , " -> Server:         " ++ show server
        , " -> Management:     " ++ show management
        , " -> Flush Interval: " ++ show interval
        , " -> Percentiles:    " ++ show percentiles
        , " -> Log:            " ++ show log
        , " -> Log Path:       " ++ show logPath
        , " -> Graphite:       " ++ show graphite
        , " -> Broadcast:      " ++ show broadcast
        , " -> Upstream:       " ++ show upstream
        ]
    show _ = ""

defaultOptions :: Options
defaultOptions = Options
    { server      = Addr "0.0.0.0" 8125
    , management  = Addr "0.0.0.0" 8126
    , interval    = 10
    , percentiles = [90]
    , log         = []
    , logPath     = "stdout"
    , graphite    = []
    , broadcast   = []
    , upstream    = []
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
    , flagReq ["interval"] (f setInterval) "INT" "Interval between key flushes to subscribed sinks"
    , flagReq ["percentiles"] (g setPercentiles) "[INT]" "Calculate the Nth percentile(s) for timers"
    , flagReq ["log"] (g setLog) "[EVENT]" "Log [receive,invalid,parse,flush] events"
    , flagReq ["log-path"] (g setLogPath) "PATH" "Log file path, or stdout"
    , flagReq ["graphite"] (g setGraphite) "[ADDR:PORT]" "Graphite hosts to deliver metrics to"
    , flagReq ["broadcast"] (g setBroadcast) "[ADDR:PORT]" "Hosts to broadcast raw unaggregated packets to"
    , flagReq ["upstream"] (g setUpstream) "[ADDR:PORT]" "Hosts to forward aggregated counters to"
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
