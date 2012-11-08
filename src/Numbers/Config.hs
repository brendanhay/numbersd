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

import Control.Monad                   (when)
import Data.Lens.Common
import Data.Lens.Template
import Data.List.Split                 (splitOn)
import Data.Monoid                     (mempty, mconcat)
import Data.Version                    (showVersion)
import Paths_numbersd                  (version)
import System.Console.CmdArgs.Explicit
import System.Environment
import System.Exit
import Numbers.Log
import Numbers.Types

import qualified Data.ByteString.Char8 as BS

data Config = Help | Version | Config
    { _listeners      :: [Uri]
    , _overview       :: Maybe Int
    , _interval       :: Int
    , _percentiles    :: [Int]
    , _logEvents      :: [String]
    , _logPath        :: String
    , _graphites      :: [Uri]
    , _graphitePrefix :: String
    , _broadcasts     :: [Uri]
    , _downstreams    :: [Uri]
    }

$(makeLens ''Config)

instance Loggable Config where
    build Config{..} = mconcat
        [ build "Configuration: \n"
        , " -> Listeners:       " ++\ _listeners
        , " -> Overview Port:   " ++\ _overview
        , " -> Flush Interval:  " ++\ _interval
        , " -> Percentile:      " ++\ _percentiles
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
    { _listeners      = [Udp (BS.pack "0.0.0.0") 8125]
    , _overview       = Nothing
    , _interval       = 10
    , _percentiles     = [90]
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
        c       -> validate c >> infoL c >> return c

info :: String -> String
info name = concat
    [ name
    , " version "
    , showVersion version
    , " (C) Brendan Hay <brendan@soundcloud.com> 2012"
    ]

validate :: Config -> IO ()
validate Config{..} = do
    check (null _listeners)   "--listeners cannot be blank"
    check (null _percentiles) "--percentiles cannot be blank"
    check (null _logPath)     "--log-path cannot be blank"
    return ()
  where
    check p m = when p $ putStrLn m >> exitWith (ExitFailure 1)
validate _ = return ()

flags :: String -> Mode Config
flags name = mode name defaultConfig "Numbers"
    (flagArg (\x _ -> Left $ "Unexpected argument " ++ x) "")
    [ flagReq ["listeners"]
      (many listeners)
      "[URI]"
      "Incoming stats address and port combinations"

    , flagReq ["overview-port"]
      (\s o -> Right $ (setL overview . Just $ read s) o)
      "URI"
      "HTTP port to serve /numbers.json on"

    , flagReq ["interval"]
      (one interval)
      "INT"
      "Interval between key flushes to subscribed sinks"

    , flagReq ["percentiles"]
      (many percentiles)
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
      "[URI]"
      "Graphite hosts to deliver metrics to"

    , flagReq ["graphite-prefix"]
      (many graphitePrefix)
      "STRING"
      "Prepended to all keys flushed to graphite"

    , flagReq ["broadcasts"]
      (many broadcasts)
      "[URI]"
      "Hosts to broadcast raw unaggregated packets to"

    , flagReq ["downstreams"]
      (many downstreams)
      "[URI]"
      "Hosts to forward aggregated counters to"

    , flagNone ["help", "h"]
      (\_ -> Help)
      "Display this help message"

    , flagVersion $ \_ -> Version
    ]
  where
    one l s  = Right . setL l (read s)
    many l s = Right . (setL l . map read $ splitOn "," s)
