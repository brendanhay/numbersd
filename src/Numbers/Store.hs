-- |
-- Module      : Numbers.Store
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Numbers.Store (
    -- * Opaque
      Store
    , runStore
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Data.Conduit             hiding (Flush)
import Numbers.Conduit
import Numbers.Types

import qualified Data.ByteString.Char8 as BS
import qualified Numbers.Map           as M

data Store = Store
    { _sinks :: [EventSink]
    , _tmap  :: M.Map Key Metric
    }

runStore :: [Int] -> Int -> [EventSink] -> TBQueue BS.ByteString -> IO ()
runStore qs n hs q = runResourceT $ sourceQueue q $$ sink
  where
    sink = bracketP (liftIO $ newStore qs n hs) (\_ -> return ())
        (\s -> awaitForever $ liftIO . flip insert s)

newStore :: [Int] -> Int -> [EventSink] -> IO Store
newStore qs n sinks = Store sinks `fmap` M.empty policy
  where
    policy     = M.Continue n f
    f key m ts = mapM_ (pushEvent sinks . Flush ts) $ calculate qs n key m

insert :: BS.ByteString -> Store -> IO ()
insert bstr Store{..} = do
    pushEvent _sinks $ Receive bstr
    forM_ (filter (not . BS.null) $ BS.lines bstr) f
  where
    f b = case decode metricParser b of
        Just (k, v) -> M.update k (return . aggregate v) _tmap
        Nothing     -> pushEvent _sinks $ Invalid bstr