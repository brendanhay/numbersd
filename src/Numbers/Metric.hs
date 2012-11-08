{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Numbers.Metric
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Metric (
      Metric(..)
    , Key(..)
    , append
    , decode
    ) where

import Control.Arrow                     (first)
import Control.Applicative        hiding (empty)
import Data.Attoparsec.ByteString
import Text.Regex.PCRE            hiding (match)

import qualified Data.Attoparsec.Char8 as PC
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set              as S

data Metric = Counter Double
            | Timer [Double]
            | Gauge Double
            | Set (S.Set Double)
              deriving (Eq, Ord, Show)

newtype Key = Key BS.ByteString
    deriving (Eq, Ord, Show)

append :: Metric -> Metric -> Metric
append (Counter x) (Counter y) = Counter $ x + y
append (Timer x)   (Timer y)   = Timer $ x ++ y
append (Gauge _) g@(Gauge _)   = g
append (Set x)     (Set y)     = Set $ x `S.union` y
append _           right       = right

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
    strip s = foldl (flip $ uncurry replace) s unsafe

unsafe :: [(Regex, BS.ByteString)]
unsafe = map (first $ makeRegex . BS.pack)
    [ ("\\s+", "_")
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