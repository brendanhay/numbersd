{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Numbers.Types
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Types (
    -- * Type Classes
      Loggable(..)

    -- * Exported Types
    , Uri(..)
    , Metric(..)
    , Key(..)

    -- * Functions
    , zero
    , aggregate
    , average
    , decode
    , metric
    ) where

import Blaze.ByteString.Builder
import Control.Arrow                     ((***), first)
import Control.Applicative        hiding (empty)
import Control.Monad
import Data.Aeson                        (ToJSON(..))
import Data.Attoparsec.ByteString
import Data.List                         (intercalate, intersperse)
import Data.Maybe
import Data.Monoid
import Data.Time.Clock.POSIX
import Text.Regex.PCRE            hiding (match)

import qualified Data.Attoparsec.Char8 as PC
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set              as S

class Loggable a where
    build :: Loggable a => a -> Builder
    (+++) :: (Loggable a, Loggable b) => a -> b -> Builder
    (++\) :: (Loggable a, Loggable b) => a -> b -> Builder
    (++&) :: (Loggable a, Loggable b) => a -> b -> Builder

    a +++ b = build a `mappend` build b
    a ++\ b = a +++ b +++ BS.pack "\n"
    a ++& b = a +++ BS.pack " " +++ b

instance Loggable Builder where
    build = id

instance Loggable BS.ByteString where
    build = copyByteString

instance Loggable Int where
    build = build . show

instance Loggable Double where
    build = build . show

instance Loggable POSIXTime where
    build = build . show . toRational

instance Loggable String where
    build = build . BS.pack

instance Loggable [String] where
    build = build . intercalate ", "

instance Loggable a => Loggable (Maybe a) where
    build (Just x) = build x
    build Nothing  = mempty

-- Investigate how to avoid overlapping instances for
-- instance Loggable a => Loggable [a] delcaration

instance Loggable [Int] where
    build = build . show

instance Loggable [Double] where
    build = build . show

-- ^

data Uri = Tcp { _host :: BS.ByteString, _port :: Int }
         | Udp { _host :: BS.ByteString, _port :: Int }

instance Read Uri where
    readsPrec _ a = return (fromJust . decode uri $ BS.pack a, "")

instance Loggable Uri where
    build (Tcp h p) = "tcp://" +++ h +++ ":" +++ p
    build (Udp h p) = "udp://" +++ h +++ ":" +++ p

instance Loggable [Uri] where
    build = mconcat . intersperse (build ", ") . map build

uri :: Parser Uri
uri = do
    s <- PC.takeTill (== ':') <* string (BS.pack "://")
    a <- PC.takeTill (== ':') <* PC.char ':'
    p <- PC.decimal :: Parser Int
    return $ case BS.unpack s of
        "tcp"  -> Tcp a p
        "udp"  -> Udp a p
        _      -> error "Unrecognized uri scheme"
                  -- ^ TODO: investigate purposeful parser failures

data Metric = Counter Double
            | Timer [Double]
            | Gauge Double
            | Set (S.Set Double)
              deriving (Eq, Ord)

instance ToJSON Metric where
    toJSON v = case v of
        (Counter n) -> toJSON n
        (Timer ns)  -> toJSON ns
        (Gauge n)   -> toJSON n
        (Set ss)    -> toJSON $ S.toAscList ss

instance Loggable Metric where
    build v = case v of
        (Counter n) -> "Counter " +++ n
        (Timer ns)  -> "Timer " +++ ns
        (Gauge n)   -> "Gauge " +++ n
        (Set ss)    -> "Set " +++ S.toAscList ss

instance Loggable [Metric] where
    build = mconcat . map (\v -> build v +++ ", ")

newtype Key = Key BS.ByteString
    deriving (Eq, Ord)

instance Loggable Key where
    build (Key k) = build k

instance Loggable [Key] where
    build = mconcat . map (\k -> build k +++ ", ")

zero :: Metric -> Bool
zero (Counter 0) = True
zero (Timer [])  = True
zero (Gauge 0)   = True
zero (Set x)     = x == S.empty
zero _           = False

aggregate :: Metric -> Metric -> Metric
aggregate (Counter x) (Counter y) = Counter $ x + y
aggregate (Timer x)   (Timer y)   = Timer $ x ++ y
aggregate (Set x)     (Set y)     = Set $ x `S.union` y
aggregate _           right       = right

average :: Metric -> Metric -> Metric
average a b@(Counter _) = Counter $ x / 2
  where
    (Counter x) = a `aggregate` b
average _ right         = right

metric :: Parser (Key, Metric)
metric = do
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
    strip s = foldl (flip $ uncurry replace) s unsafe

decode :: Parser a -> BS.ByteString -> Maybe a
decode p bstr = maybeResult $ feed (parse p bstr) BS.empty

unsafe :: [(Regex, BS.ByteString)]
unsafe = map (first makeRegex . join (***) BS.pack) rs
  where
    rs = [ ("\\s+", "_")
         , ("\\/", "-")
         , ("[^a-zA-Z_\\-0-9\\.]", "")
         ]

replace :: Regex -> BS.ByteString -> BS.ByteString -> BS.ByteString
replace regex rep = go
  where
    go s = case match regex s of
        Just (a, _, c) -> a `BS.append` rep `BS.append` go c
        _              -> s

match :: Regex -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString, BS.ByteString)
match = matchM