-- |
-- Module      : Numbers.Whisper
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Whisper (
    -- * Opaque
      Whisper
    , newWhisper

    -- * Operations
    , insert

    -- * Formatters
    , json
    , text
    ) where

import Blaze.ByteString.Builder        (Builder, copyLazyByteString)
import Control.Arrow                   (second)
import Control.Monad                   (liftM)
import Data.Aeson               hiding (json)
import Data.Text.Encoding              (decodeUtf8)
import Numbers.Types
import Numbers.Whisper.Series          (Resolution, Series, Step)

import qualified Numbers.TMap           as M
import qualified Numbers.Whisper.Series as S

data Whisper = Whisper
    { res  :: Resolution
    , step :: Step
    , db   :: M.TMap Key Series
    }

newWhisper :: Resolution -> Step -> IO Whisper
newWhisper r s = Whisper (r `div` s) s `liftM` M.empty
-- ^ Investigate implications of div absolute rounding torwards zero

insert :: Key -> Metric -> Time -> Whisper -> IO ()
insert key val ts = update key ts v
  where
    v = case val of
        (Counter d) -> d
        (Timer ds)  -> sum ds / fromIntegral (length ds)
        (Gauge d)   -> d
        (Set _)     -> 1

json :: Time -> Time -> Whisper -> IO Builder
json from to w = fetch from to w >>=
    return . copyLazyByteString . encode . object . map f
  where
    f (Key k, s) = decodeUtf8 k .= toJSON s

text :: Time -> Time -> Whisper -> IO Builder
text from to w = fetch from to w >>= return . build . map f
  where
    f (Key k, s) = k +++ "," +++ s +++ "\n"

update :: Key -> Time -> Double -> Whisper -> IO ()
update key ts val Whisper{..} = M.update key f db
  where
    f = maybe (return $ S.create res step ts val) (return . S.update ts val)

fetch :: Time -> Time -> Whisper -> IO [(Key, Series)]
fetch from to w = map (second (S.fetch from to)) `liftM` M.toList (db w)
