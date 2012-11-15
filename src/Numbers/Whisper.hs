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

module Numbers.Whisper where

import Blaze.ByteString.Builder (Builder, copyLazyByteString)
import Data.Aeson
import Numbers.Types
import Control.Arrow            (second)
import Data.Text.Encoding       (decodeUtf8)
import Numbers.Whisper.Series   (Series)

import qualified Data.Map                   as M
import qualified Numbers.Whisper.Series     as S

data Whisper = Whisper
    { _db     :: M.Map Key Series
    , _retain :: Int
    , _step   :: Int
    }

json :: Time -> Time -> Whisper -> Builder
json from to = copyLazyByteString . encode . object . map f . fetch from to
  where
    f (Key k, s) = decodeUtf8 k .= toJSON s

text :: Time -> Time -> Whisper -> Builder
text from to = build . map f . fetch from to
  where
    f (Key k, s) = k +++ "," +++ s +++ "\n"

fetch :: Time -> Time -> Whisper -> [(Key, Series)]
fetch from to = map (second (S.fetch from to)) . M.toList . _db

newWhisper :: Int -> Int -> Whisper
newWhisper res step = Whisper M.empty (res `div` step) step
-- ^ Investigate implications of div absolute rounding torwards zero

update :: Whisper -> Key -> Time -> Double -> Whisper
update w@Whisper{..} key ts val = w { _db = M.alter (Just . f) key _db }
  where
    f = maybe (S.create _retain _step ts val) (S.update ts val)
