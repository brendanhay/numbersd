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
import Data.Aeson               hiding (json)
import Data.Text.Encoding              (decodeUtf8)
import Numbers.Types
import Numbers.Whisper.Series          (Resolution, Series, Step)

import qualified Data.Map               as M
import qualified Numbers.Whisper.Series as S

data Whisper = Whisper
    { res  :: Resolution
    , step :: Step
    , db   :: M.Map Key Series
    }

newWhisper :: Resolution -> Step -> Whisper
newWhisper r s = Whisper (r `div` s) s M.empty
-- ^ Investigate implications of div absolute rounding torwards zero

insert :: Key -> Metric -> Time -> Whisper -> Whisper
insert key val ts = update key ts v
  where
    v = case val of
        (Counter d) -> d
        (Timer ds)  -> sum ds / fromIntegral (length ds)
        (Gauge d)   -> d
        (Set _)     -> 1

json :: Time -> Time -> Whisper -> Builder
json from to = copyLazyByteString . encode . object . map f . fetch from to
  where
    f (Key k, s) = decodeUtf8 k .= toJSON s

text :: Time -> Time -> Whisper -> Builder
text from to = build . map f . fetch from to
  where
    f (Key k, s) = k +++ "," +++ s +++ "\n"

update :: Key -> Time -> Double -> Whisper -> Whisper
update key ts val w@Whisper{..} = w { db = M.alter (Just . f) key db }
  where
    f = maybe (S.create res step ts val) (S.update ts val)

fetch :: Time -> Time -> Whisper -> [(Key, Series)]
fetch from to = map (second (S.fetch from to)) . M.toList . db
