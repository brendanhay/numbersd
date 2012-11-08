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

import Data.Lens.Common
import Data.Lens.Template
import Data.List.Split                 (splitOn)
import Data.Monoid (mempty, mconcat)
import Data.Version                    (showVersion)
import Paths_numbersd                  (version)
import System.Console.CmdArgs.Explicit
import System.Environment
import System.Exit
import Numbers.Log
import Numbers.Types

import qualified Data.ByteString.Char8 as BS

data Config = Help | Version | Config
    { _listener       :: Addr
    , _overview         :: Maybe Addr
    , _interval       :: Int
    , _percentile     :: [Int]
    , _logEvents      :: [String]
    , _logPath        :: String
    , _graphites      :: [Addr]
    , _graphitePrefix :: String
    , _broadcasts     :: [Addr]
    , _downstreams    :: [Addr]
    }

$(makeLens ''Config)

instance Loggable Config where
    build Config{..} = mconcat
        [ build "Configuration: \n"
        , " -> UDP Listener:    " ++\ _listener
        , " -> HTTP Overview:     " ++\ _overview
        , " -> Flush Interval:  " ++\ _interval
        , " -> Percentile:      " ++\ _percentile
        , " -> Log Events:      " ++\ _logEvents
        , " -> Log Path:        " ++\ _logPath
        , " -> Graphites:       " ++\ _graphites
        , " -> Graphite Prefix: " ++\ _graphitePrefix
        , " -> Broadcasts:      " ++\ _broadcasts
        , " -> Downstreams:     " +++ _downstreams
        ]
    build _ = mempty

defaultConfig :: Config
defaultConfig = Config
    { _listener       = Addr (BS.pack "0.0.0.0") 8125
    , _overview        = Nothing
    , _interval       = 10
    , _percentile     = [90]
    , _logEvents      = ["flush"]
    , _logPath        = "stdout"
    , _graphites      = []
    , _graphitePrefix = "stats"
    , _broadcasts     = []
    , _downstreams    = []
    }

parseConfig :: IO Config
parseConfig = do
    a <- getArgs
    n <- getProgName
    case processValue (flags n) a of
        Help    -> print (helpText [] HelpFormatOne $ flags n) >> exitSuccess
        Version -> print (info n) >> exitSuccess
        opts    -> infoL opts >> return opts

info :: String -> String
info name = concat
    [ name
    , " version "
    , showVersion version
    , " (C) Brendan Hay <brendan@soundcloud.com> 2012"
    ]

flags :: String -> Mode Config
flags name = mode name defaultConfig "Numbers"
    (flagArg (\x _ -> Left $ "Unexpected argument " ++ x) "")
    [ flagReq ["udp"]
      (one listener)
      "ADDR:PORT"
      "Incoming stats UDP address and port"

    , flagReq ["tcp"]
      (one listener)
      "ADDR:PORT"
      "Incoming stats TCP address and port"

    , flagReq ["overview"]
      (\s o -> Right $ (setL overview . Just $ read s) o)
      "ADDR:PORT"
      "HTTP address and port for /numbers.json"

    , flagReq ["interval"]
      (one interval)
      "INT"
      "Interval between key flushes to subscribed sinks"

    , flagReq ["percentiles"]
      (many percentile)
      "[INT]"
      "Calculate the Nth percentile(s) for timers"

    , flagReq ["log-events"]
      (\s o -> Right $ (logEvents ^= splitOn "," s) o)
      "[EVENT]"
      "Lomany [receive,invalid,parse,flush] events"

    , flagReq ["log-path"]
      (\s o -> Right $ (logPath ^= s) o)
      "PATH"
      "Lomany file path, or stdout"

    , flagReq ["graphites"]
      (many graphites)
      "[ADDR:PORT]"
      "Graphite hosts to deliver metrics to"

    , flagReq ["graphite-prefix"]
      (many graphitePrefix)
      "STRING"
      "Prepended to all keys flushed to graphite"

    , flagReq ["broadcasts"]
      (many broadcasts)
      "[ADDR:PORT]"
      "Hosts to broadcast raw unaggregated packets to"

    , flagReq ["downstreams"]
      (many downstreams)
      "[ADDR:PORT]"
      "Hosts to forward aggregated counters to"

    , flagNone ["help", "h"]
      (\_ -> Help)
      "Display this help message"

    , flagVersion $ \_ -> Version
    ]
  where
    one l s  = Right . setL l (read s)
    many l s = Right . (setL l . map read $ splitOn "," s)
