-- |
-- Module      : Vodki.Regex
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Vodki.Regex (
    -- * A la carte
      comments
    , unsafe'

    -- * Functions
    , compile
    , compile'
    , replace
    ) where

import Text.Regex.PCRE

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

data Replace a = Replace (a -> Maybe (a, a, a)) a (a -> a -> a)

comments :: BL.ByteString -> Replace BL.ByteString
comments = compile "/\\*.+\\*/"

unsafe' :: [Replace BS.ByteString]
unsafe' = map (uncurry compile')
    [ ("\\s+", "_")
    , ("\\/", "-")
    , ("[^a-zA-Z_\\-0-9\\.]", "")
    ]

compile :: BL.ByteString -> BL.ByteString -> Replace BL.ByteString
compile r rep = Replace (matchM $! makeRegexOpts compDotAll execBlank r) rep BL.append

compile' :: BS.ByteString -> BS.ByteString -> Replace BS.ByteString
compile' r rep = Replace (matchM $! (makeRegex r :: Regex)) rep BS.append

replace :: Replace a -> a -> a
replace (Replace m rep app) = go
  where
    go s = case m s of
        Just (a, _, c) -> a `app` rep `app` go c
        _              -> s
