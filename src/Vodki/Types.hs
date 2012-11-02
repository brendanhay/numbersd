{-# LANGUAGE ExistentialQuantification, TupleSections #-}

-- |
-- Module      : Vodki.Types
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Vodki.Types where

import Control.Applicative        hiding (empty)
import Control.Monad                     (liftM)
import Data.Attoparsec.ByteString
import Data.Monoid                       (mappend)
import Data.Time.Clock.POSIX
import Vodki.Regex

import qualified Control.Concurrent.Chan.Split as C
import qualified Data.Attoparsec.Char8         as PC
import qualified Data.ByteString.Char8         as BS
import qualified Data.Set                      as S
import qualified Network.Socket                as S

data Metric = Counter Double
            | Timer [Double]
            | Gauge Double
            | Set (S.Set Double)
              deriving (Eq, Ord, Show)

newtype Key = Key { getKey :: BS.ByteString }
    deriving (Eq, Ord, Show)

decode :: BS.ByteString -> Maybe (Key, Metric)
decode bstr = maybeResult $ feed (parse parser bstr) ""

parser :: Parser (Key, Metric)
parser = do
    k <- Key . strip <$> PC.takeTill (== ':') <* PC.char ':'
    v <- PC.double <* PC.char '|'
    t <- PC.anyChar
    r <- optional (PC.char '|' *> PC.char '@' *> PC.double)
    return . (k,) $
        case t of
            'm' -> Timer [v]
            'g' -> Gauge v
            's' -> Set $ S.singleton v
            _   -> Counter $ maybe v (\n -> v * (1 / n)) r -- Div by zero
  where
    strip s = foldl (flip replace) s unsafe'

append :: Metric -> Metric -> Metric
append (Counter x) (Counter y) = Counter $ x + y
append (Timer x)   (Timer y)   = Timer $ x ++ y
append (Gauge _) g@(Gauge _)   = g
append (Set x)     (Set y)     = Set $ x `S.union` y
append _           right       = right
-- ^ Last written (flushed) wins, same as graphite
-- should return Either (succ, fail) succ ?

data Event = Insert BS.ByteString
           | Invalid BS.ByteString
           | Bucket Key Metric
           | Flush Key Metric POSIXTime Int
             deriving (Eq, Ord, Show)

class Sink a where
    emit :: a -> Event -> IO ()

data AnySink = forall a. Sink a => Sink a

instance Sink AnySink where
    emit (Sink s) = emit s

data Debug = Debug

debug :: AnySink
debug = Sink Debug

instance Sink Debug where
    emit _ (Bucket k v) = putStrLn $ "Debug: " ++ show k ++ " " ++ show v
    emit _ _ = return ()

data Repeater = Repeater

repeater :: AnySink
repeater = Sink Repeater

instance Sink Repeater where
    emit _ (Insert s) = putStrLn $ "Repeater: " ++ BS.unpack s
    emit _ _ = return ()

data Graphite = Graphite

graphite :: AnySink
graphite = Sink Graphite

instance Sink Graphite where
    emit _ (Flush k v ts n) = putStrLn $ "Graphite: " ++ show k ++ " " ++ show v ++ " " ++ show ts
    emit _ _ = return ()
