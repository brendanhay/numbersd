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
    , sbuild

    -- * Exported Types
    , Time(..)
    , Uri(..)
    , Key(..)
    , Metric(..)

    -- * Functions
    , currentTime
    , zero
    , aggregate
    , flatten
    , calculate
    , metricParser
    , decode
    ) where

import Blaze.ByteString.Builder
import Control.Arrow                     ((***), first)
import Control.Applicative        hiding (empty)
import Control.Monad
import Data.Aeson                        (ToJSON(..))
import Data.Attoparsec.ByteString
import Data.List                  hiding (sort)
import Data.Maybe
import Data.Monoid                       (mconcat, mappend, mempty)
import Data.Time.Clock.POSIX
import Numeric                           (showFFloat)
import Statistics.Function               (sort)
import Statistics.Sample
import Text.Regex.PCRE            hiding (match)

import qualified Data.Attoparsec.Char8 as PC
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set              as S
import qualified Data.Vector           as V

class Loggable a where
    build :: Loggable a => a -> Builder

    (&&&) :: (Loggable a, Loggable b) => a -> b -> Builder
    (<&&)  :: Loggable a => String -> a -> Builder
    (&&>)  :: Loggable a => a -> String -> Builder

    infixr 7 &&&
    infixr 9 <&&
    infixr 8 &&>

    a &&& b = build a `mappend` build b
    a <&& b  = build a &&& b
    a &&> b  = a &&& build b

sbuild :: String -> Builder
sbuild = build . BS.pack
{-# INLINE sbuild #-}

instance Loggable Builder where
    build = id

instance Loggable [Builder] where
    build = mconcat

instance Loggable BS.ByteString where
    build = copyByteString

instance Loggable Int where
    build = build . show

instance Loggable Double where
    build n = build $ showFFloat (Just 1) n ""

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

instance Loggable (V.Vector Double) where
    build = build . V.toList

newtype Time = Time Int
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

instance Loggable Time where
    build (Time n) = build $ show n

currentTime :: IO Time
currentTime = (Time . truncate) `liftM` getPOSIXTime

data Uri = Tcp { _host :: BS.ByteString, _port :: Int }
         | Udp { _host :: BS.ByteString, _port :: Int }

instance Read Uri where
    readsPrec _ a = return (fromJust . decode uriParser $ BS.pack a, "")

instance Loggable Uri where
    build (Tcp h p) = "tcp://" <&& h &&& ":" <&& p
    build (Udp h p) = "udp://" <&& h &&& ":" <&& p

instance Loggable [Uri] where
    build = mconcat . intersperse (sbuild ", ") . map build

uriParser :: Parser Uri
uriParser = do
    s <- PC.takeTill (== ':') <* string "://"
    a <- PC.takeTill (== ':') <* PC.char ':'
    p <- PC.decimal :: Parser Int
    return $ case BS.unpack s of
        "tcp"  -> Tcp a p
        "udp"  -> Udp a p
        _      -> error "Unrecognized uri scheme"
                  -- ^ TODO: investigate purposeful parser failures

newtype Key = Key BS.ByteString
    deriving (Eq, Ord, Show)

instance Loggable Key where
    build (Key k) = build k

instance Loggable [Key] where
    build = mconcat . intersperse (sbuild ",") . map build

keyParser :: Parser Key
keyParser = Key . strip <$> PC.takeTill (== ':') <* PC.char ':'
  where
    strip s = foldl (flip $ uncurry replace) s unsafe

data Metric = Counter Double
            | Timer [Double]
            | Gauge Double
            | Set (S.Set Double)
              deriving (Eq, Ord, Show)

instance ToJSON Metric where
    toJSON m = case m of
        (Counter v) -> toJSON v
        (Timer  vs) -> toJSON vs
        (Gauge   v) -> toJSON v
        (Set    ss) -> toJSON $ S.toAscList ss

instance Loggable Metric where
    build m = case m of
        (Counter v) -> "Counter " <&& v
        (Timer  vs) -> "Timer "   <&& vs
        (Gauge   v) -> "Gauge "   <&& v
        (Set    ss) -> "Set "     <&& S.toAscList ss

instance Loggable [Metric] where
    build = mconcat . intersperse (sbuild ",") . map build

metricParser :: Parser (Key, Metric)
metricParser = do
    k <- keyParser
    v <- PC.double <* PC.char '|'
    t <- PC.anyChar
    r <- optional (PC.char '|' *> PC.char '@' *> PC.double)
    return . (k,) $
        case t of
            'm' -> Timer [v]
            'g' -> Gauge v
            's' -> Set $ S.singleton v
            _   -> Counter $ maybe v (\n -> v * (1 / n)) r -- Div by zero

zero :: Metric -> Bool
zero (Counter 0) = True
zero (Timer  []) = True
zero (Gauge   0) = True
zero (Set    ss) = S.null ss
zero _           = False

aggregate :: Metric -> Metric -> Metric
aggregate (Counter x) (Counter y) = Counter $ x + y
aggregate (Timer   x) (Timer   y) = Timer   $ x ++ y
aggregate (Set     x) (Set     y) = Set     $ x `S.union` y
aggregate _           right       = right

data Point = P BS.ByteString BS.ByteString Double
           | S BS.ByteString Double
    deriving (Show)

flatten :: BS.ByteString -> Key -> Point -> (BS.ByteString, Double)
flatten p (Key k) pnt = (BS.concat $ intersperse "." (p:xs), v')
  where
    (xs, v') = case pnt of
        P a b v -> ([a, k, b], v)
        S a v   -> ([a, k], v)

calculate :: [Int] -> Int -> Metric -> [Point]
calculate _ n (Counter v) =
    [ S "counters" $ v / (fromIntegral n / 1000)
    , P "counters" "count" v
    ]
calculate _ _ (Gauge v) =
    [ S "gauges" v ]
calculate _ _ (Set ss) =
    [ P "sets" "count" . fromIntegral $ S.size ss ]
calculate qs _ (Timer vs) = concatMap (`quantile` xs) qs ++
    [ P "timers" "std"   $ stdDev xs
    , P "timers" "upper" $ V.last xs
    , P "timers" "lower" $ V.head xs
    , P "timers" "count" . fromIntegral $ V.length xs
    , P "timers" "sum"   $ V.sum xs
    , P "timers" "mean"  $ mean xs
    ]
  where
    xs = sort $ V.fromList vs

quantile :: Int -> V.Vector Double -> [Point]
quantile q xs =
    [ P "timers" (a "mean_")  $ mean ys
    , P "timers" (a "upper_") $ V.last ys
    , P "timers" (a "sum_")   $ V.sum ys
    ]
  where
    ys = V.take n xs
    n  = round $ fromIntegral q / 100 * (fromIntegral $ V.length xs :: Double)
    a  = flip BS.append (BS.pack $ show q)

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
