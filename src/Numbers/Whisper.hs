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
    , fetch
    , keys
    ) where

import Control.Applicative             ((<$>))
import Control.Arrow                   (second)
import Control.Monad                   (liftM)
import Data.Maybe
import Numbers.Types
import Numbers.Whisper.Series          (Resolution, Series, Step)

import qualified Numbers.Map            as M
import qualified Numbers.Whisper.Series as S

data Whisper = Whisper
    { _res   :: Resolution
    , _step  :: Step
    , _db    :: M.Map Key Series
    }

newWhisper :: Resolution -> Step -> IO Whisper
newWhisper res step = do
    db <- M.empty $ M.Reset res (\_ _ _ -> return ())
    return $! Whisper (res `div` step) step db
-- ^ Investigate implications of div absolute rounding torwards zero

insert :: Time -> Point -> Whisper -> IO ()
insert ts (P k v) Whisper{..} =
    M.update k (maybe (S.create _res _step ts v) (S.update ts v)) _db

fetch :: Time -> Time -> Whisper -> Maybe [Key] -> IO [(Key, Series)]
fetch from to Whisper{..}  mks = map (second (S.fetch from to)) `liftM`
  case mks of
   Nothing -> M.toList _db
   Just ks -> catMaybes <$> mapM f ks
  where
    f :: Key -> IO (Maybe (Key, Series))
    f k = do
      mv <- M.lookup k _db
      return $ (\v -> (k, v)) <$> mv

keys :: Whisper -> IO [Key]
keys = M.keys . _db