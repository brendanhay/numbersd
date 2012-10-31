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
      Config(..)

    -- * Functions
    , getConfig
    ) where

import Control.Applicative
import Control.Monad          (liftM, unless)
import Data.Aeson
import Data.Version           (showVersion)
import Paths_vodki            (version)
import System.Console.CmdArgs
import System.Directory       (doesFileExist)
import System.Environment     (getProgName)
import Vodki.Regex

import qualified Data.ByteString.Lazy.Char8 as BL

data Options = Options
    { _config :: FilePath
    } deriving (Data, Typeable)

defaultOptions :: Options
defaultOptions = Options
    { _config = "./vodki.json"
        &= name "config"
        &= typ  "PATH"
        &= help "json configuration file, default ./vodki.json"
        &= explicit
    }

data Config = Config
    { _listenPort       :: Int
    , _manageAddress    :: String
    , _managePort       :: Int
    , _debug            :: Bool
    , _debugInterval    :: Int
    , _dumpMessages     :: Bool
    , _flushInterval    :: Int
    , _percentThreshold :: [Double]
    , _graphiteHost     :: Maybe String
    , _graphitePort     :: Maybe Int
    } deriving (Eq, Ord)

defaultConfig :: Config
defaultConfig = Config
    { _listenPort       = 8125
    , _manageAddress    = "0.0.0.0"
    , _managePort       = 8126
    , _debug            = False
    , _debugInterval    = 1
    , _dumpMessages     = False
    , _flushInterval    = 10
    , _percentThreshold = [90]
    , _graphiteHost     = Nothing
    , _graphitePort     = Nothing
    }

instance Show Config where
    show Config{..} = unlines
        [ "Configuration: "
        , " -> Port:              " ++ show _listenPort
        , " -> Mgmt Address:      " ++ show _manageAddress
        , " -> Mgmt Port:         " ++ show _managePort
        , " -> Debug:             " ++ show _debug
        , " -> Debug Interval:    " ++ show _debugInterval
        , " -> Dump Messages:     " ++ show _dumpMessages
        , " -> Flush Interval:    " ++ show _flushInterval
        , " -> Percent Threshold: " ++ show _percentThreshold
        , " -> Graphite Host:     " ++ show _graphiteHost
        , " -> Graphite Port:     " ++ show _graphitePort
        ]

instance FromJSON Config where
    parseJSON (Object o) = Config
        <$> o .:? "port"             .!= _listenPort
        <*> o .:? "mgmt_address"     .!= _manageAddress
        <*> o .:? "mgmt_port"        .!= _managePort
        <*> o .:? "debug"            .!= _debug
        <*> o .:? "debugInterval"    .!= _debugInterval
        <*> o .:? "dumpMessages"     .!= _dumpMessages
        <*> o .:? "flushInterval"    .!= _flushInterval
        <*> o .:? "percentThreshold" .!= _percentThreshold
        <*> o .:? "graphiteHost"
        <*> o .:? "graphitePort"
      where
        Config{..} = defaultConfig
    parseJSON _ = empty

parseOptions :: IO Options
parseOptions = do
    app <- getProgName
    let ver = summary $ concat [app, ": ", showVersion version]
    cmdArgs $ defaultOptions
        &= versionArg [explicit, name "version", name "v", ver]
        &= summary ""
        &= helpArg [explicit, name "help", name "h"]
        &= program ("Usage: " ++ app)

getConfig :: IO Config
getConfig = do
    (Options f) <- parseOptions
    p <- doesFileExist f
    unless p (error $ "No configuration file not found at " ++ f)
    s <- replace (comments "") `liftM` BL.readFile f
    maybe (error $ "Invalid json in the configuration file " ++ f)
          return
          (decode' s :: Maybe Config)
