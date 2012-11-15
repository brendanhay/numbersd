-- |
-- Module      : Benchmarks
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Benchmarks where

import Criterion.Main
import Numbers.Types

main :: IO ()
main = return ()

    -- let a = ..
    --     b = ..
    --     c = ..
    -- evaluate (a `seq` a' `seq` b `seq` b' `seq` ())
    -- defaultMain
    --    [ bgroup "listArray"
    --        [ bench "UArray"   $ whnf aListArray l
    --        , bench "BitArray" $ whnf bListArray l
    --        ]
    --    , bgroup "elems"
    --        [ bench "UArray"   $ whnf aElems a
    --        , bench "BitArray" $ whnf bElems b
    --        ]
    --    ]


-- sCreate ::

-- sInsert :: Series

times :: IO [Time]
times = do
    ts <- currentTime
    return $! map ((ts +) . Time) steps

steps :: [Int]
steps = take 100 . scanl1 (+) $ repeat 10

