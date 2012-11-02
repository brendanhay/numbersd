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
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Data.Attoparsec.ByteString
import Data.Maybe
import Data.Time.Clock.POSIX
import Vodki.Regex

import qualified Control.Concurrent.Chan.Split as C
import qualified Data.Attoparsec.Char8         as PC
import qualified Data.ByteString.Char8         as BS
import qualified Data.Map                      as M
import qualified Data.Set                      as S

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

data Store = Store
    { delay  :: Int
    , events :: C.SendPort Event
    , tvar   :: TVar (M.Map Key (TVar Metric))
    }

type Vodki a = ReaderT Store IO a

runVodki :: Int -> Vodki a -> IO a
runVodki n vodki = do
    s <- Store n <$> C.newSendPort <*> atomically (newTVar M.empty)
    runReaderT vodki s

attachSink :: Sink a => a -> Vodki ()
attachSink sink = do
    Store{..} <- ask
    source <- liftIO $ C.listen events
    liftIO . void . forkIO . forever $ C.receive source >>= emit sink

storeMetric :: BS.ByteString -> Vodki ()
storeMetric bstr = ask >>= liftIO . flip insert bstr

insert :: Store -> BS.ByteString -> IO ()
insert s@Store{..} bstr = do
    C.send events $ Insert bstr
    forM_ (filter (not . BS.null) $ BS.lines bstr) f
  where
    f b = case decode b of
        Just (k, v) -> bucket s k v
        Nothing     -> C.send events $ Invalid b

bucket :: Store -> Key -> Metric -> IO ()
bucket s@Store{..} key val = do
    C.send events $ Bucket key val
    m <- readTVarIO tvar
    case M.lookup key m of
        Just v  -> atomically $ modifyTVar' v (append val)
        Nothing -> do
            atomically $ do
                v <- newTVar val
                writeTVar tvar $! M.insert key v m
            flush s key

flush :: Store -> Key -> IO ()
flush s@Store{..} key = void . forkIO $ do
    threadDelay n
    v  <- delete s key
    ts <- getPOSIXTime
    C.send events $ Flush key v ts delay
  where
    n = delay * 1000000

delete :: Store -> Key -> IO Metric
delete Store{..} key = atomically $ do
    m <- readTVar tvar
    writeTVar tvar $! M.delete key m
    readTVar (fromJust $ M.lookup key m)