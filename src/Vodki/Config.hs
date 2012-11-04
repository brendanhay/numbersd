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
      Addr(..)
    , Options(..)

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

data Addr = Addr String Int

instance Read Addr where
    readsPrec _ a = do
        (h, b)   <- lex a
        (":", c) <- lex b
        (p, d)   <- lex c
        return (Addr h $ read p, d)

instance Show Addr where
    show (Addr h p) = h ++ ":" ++ show p

data Options = Help | Version | Options
    { server      :: Addr
    , management  :: Addr
    , interval    :: Int
    , percentiles :: [Int]
    , console     :: [Addr]
    , graphite    :: [Addr]
    , repeater    :: [String]
    , statsd      :: [String]
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
        Help    -> print (helpText [] HelpFormatOne $ flags n) >> ok
        Version -> print (info n) >> ok
        opts    -> putStr (show opts) >> return opts
  where
    ok = exitWith ExitSuccess

flags :: String -> Mode Options
flags name = mode name defaultOptions "Vodki"
    (flagArg (\x _ -> Left $ "Unexpected argument " ++ x) "")
    [ flagReq ["server"] (f setServer) "HOST:PORT" "server"
    , flagReq ["management"] (f setManagement) "HOST:PORT" "management"
    , flagReq ["interval"] (f setInterval) "SECONDS" "interval"
    , flagReq ["percentiles"] (g setPercentiles) "[INT]" "percentiles"
    , flagReq ["console"] (g setConsole) "[EVENT]" "where EVENT is a list of receive,invalid,parse,flush"
    , flagReq ["graphite"] (g setGraphite) "[HOST:PORT]" "graphite"
    , flagReq ["repeater"] (g setRepeater) "[HOST:PORT]" "repeater"
    , flagReq ["statsd"] (g setStatsd) "[HOST:PORT]" "statsd"
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
