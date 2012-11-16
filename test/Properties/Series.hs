-- |
-- Module      : Properties.Series
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Properties.Series (seriesProperties) where

import Numbers.Whisper.Series
import Numbers.Types
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

seriesProperties :: Test
seriesProperties = testGroup "series"
    [ testProperty "points length equals resolution" prop_points_length_equals_resolution
    , testProperty "end interval equals create time" prop_end_equals_create_time
    , testProperty "start - end diff equals resolution * step" prop_start_end_equals_resolution_step
    , testProperty "start equals end - resolution * step" prop_start_equals_end_resolution_step
    , testProperty "new point increments end interval" prop_new_point_increments_end_interval
    , testProperty "far future time discards existing points" prop_future_time_discards_points
    , testProperty "orders points by their insertion time" prop_ordered_by_insertion_time
--    , testProperty "fetch from truncates results" prop_fetch_from_truncates_result
    ]

prop_points_length_equals_resolution :: Series -> Bool
prop_points_length_equals_resolution series =
   resolution series == length (points series)

prop_end_equals_create_time :: Time -> Series -> Bool
prop_end_equals_create_time ts series =
    toInterval s ts == end (create (resolution series) s ts 0)
  where
    s = step series

prop_start_end_equals_resolution_step :: Series -> Bool
prop_start_end_equals_resolution_step series =
    fromIntegral (end series - start series) == (resolution series * step series)

prop_start_equals_end_resolution_step :: Series -> Bool
prop_start_equals_end_resolution_step series =
    start series == (end series - fromIntegral (resolution series * step series))

prop_new_point_increments_end_interval :: Double -> Int -> Series -> Bool
prop_new_point_increments_end_interval x y series =
    toInterval s ts == end (update ts x series)
  where
    ts = Time $ fromIntegral (end series) + (y `mod` s)
    s  = step series

prop_future_time_discards_points :: Double -> NonNegative Int -> Series -> Bool
prop_future_time_discards_points x (NonNegative y) series =
    (x : replicate (r - 1) 0) == points (update ts x series)
  where
    ts = Time $ fromIntegral (end series) + (r * step series) + y
    r  = resolution series

prop_ordered_by_insertion_time :: Series -> Property
prop_ordered_by_insertion_time series =
    forAll (vector $ resolution series) $ \xs ->
        reverse xs == points (foldl upd series xs)
  where
    upd s v = update (incr s) v s
    incr s  = fromIntegral (end s) + fromIntegral (step s)

instance Arbitrary Series where
    arbitrary = do
        l <- choose (1, maxResolution)
        Positive s <- arbitrary
        t <- arbitrary
        NonNegative v <- arbitrary
        return $ create l s t v

instance Arbitrary Time where
    arbitrary = do
        NonNegative n <- arbitrary
        return $ Time n
