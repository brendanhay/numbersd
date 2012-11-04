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

import Control.Applicative
import Control.Monad                   (liftM)
import Data.Setters
import Data.Version                    (showVersion)
import Paths_vodki                     (version)
import System.Console.CmdArgs.Explicit
import System.Directory                (doesFileExist)
import System.Environment
import System.Exit
import Vodki.Regex

import qualified Data.ByteString.Lazy.Char8 as BL

data Options = Help | Version | Options
    { server      :: String
    , management  :: String
    , interval    :: Int
    , percentiles :: [Int]
    , console     :: [String]
    , graphite    :: [String]
    , repeater    :: [String]
    , statsd      :: [String]
    } deriving (Show)

$(declareSetters ''Options)

defaultOptions :: Options
defaultOptions = Options
    { server      = "0.0.0.0:8125"
    , management  = "0.0.0.0:8126"
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
        opts    -> return opts
  where
    ok = exitWith ExitSuccess

flags :: String -> Mode Options
flags name = mode name defaultOptions "Vodki" err
    [ flagReq ["server"] (upd setServer) "HOST:PORT" "server"
    , flagReq ["management"] (upd setManagement) "HOST:PORT" "management"
    , flagReq ["interval"] (upd setInterval . read) "SECONDS" "interval"
    , flagReq ["percentiles"] (\_ o -> Right o) "[INT]" "percentiles"
    , flagReq ["console"] (\_ o -> Right o) "[EVENT]" "where EVENT is a list of receive,invalid,parse,flush"
    , flagReq ["graphite"] (\_ o -> Right o) "[HOST:PORT]" "graphite"
    , flagReq ["repeater"] (\_ o -> Right o) "[HOST:PORT]" "repeater"
    , flagReq ["statsd"] (\_ o -> Right o) "[HOST:PORT]" "statsd"
    , flagNone ["help", "h"] (\_ -> Help) "Display this help message"
    , flagVersion $ \_ -> Version
    ]
  where
    upd f s = Right . f s
    err = flagArg (\x _ -> Left $ "Unexpected argument " ++ x) ""

info :: String -> String
info name = concat
    [ name
    , " version "
    , showVersion version
    , " (C) Brendan Hay <brendan@soundcloud.com> 2012"
    ]
