{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import Control.Applicative ((<$>))
import Numbers.Whisper.Series
import Properties.Generators                ()
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck


seriesProperties :: Test
seriesProperties = testGroup "time series"
  [ testGroup "create" [
      testProperty "input resolution used by create" prop_input_resolution_used_by_create
    , testProperty "input step used by create" prop_input_step_used_by_create
    , testProperty "input step used by create" prop_input_step_used_by_create
    , testProperty "end is less than or equal to create time" prop_end_less_than_or_equal_to_create_time
    , testProperty "end is within step of create time" prop_end_within_step_of_create_time
    , testProperty "last value equals create value" prop_last_value_equals_create_value
    , testProperty "total values equals create value" prop_total_values_equals_create_value
    ]
  , testGroup "update" [
      testProperty "resolution preserved by update" prop_resolution_preserved_by_update
    , testProperty "step preserved by update" prop_step_preserved_by_update
    , testProperty "old values ignored by update" prop_old_values_ignored_by_update
    , testProperty "later times move end along" prop_later_times_move_end_along_in_update
    , testProperty "new ends are less than or equal to update time" prop_new_end_less_than_or_equal_to_update_time
    , testProperty "new ends are within step of create time" prop_new_end_within_step_of_create_time
    , testProperty "for new ends last value equals update value" prop_new_end_last_value_equals_update_value
    , testProperty "update between start and end adds value" prop_update_between_start_and_end_adds_value
    ]
  , testGroup "series" [
      testProperty "end is divisible by step" prop_end_divisible_by_step
    , testProperty "start - end diff equals resolution * step" prop_start_end_equals_resolution_step
    , testProperty "start equals end - resolution * step" prop_start_equals_end_resolution_step
    , testProperty "values length equals resolution" prop_values_length_equals_resolution
    , testProperty "orders values by their insertion time" prop_ordered_by_insertion_time
    ]
  , testGroup "fetch" [

    ]
  ]

prop_input_resolution_used_by_create :: SeriesCreate -> Bool
prop_input_resolution_used_by_create sc =
  inputCRes sc == outputCRes sc

prop_input_step_used_by_create :: SeriesCreate -> Bool
prop_input_step_used_by_create sc =
  inputCStep sc == outputCStep sc

prop_end_less_than_or_equal_to_create_time :: SeriesCreate -> Bool
prop_end_less_than_or_equal_to_create_time sc =
  fromIntegral (outputCEnd sc) <= (fromIntegral (inputCTime sc) :: Int)

prop_end_within_step_of_create_time :: SeriesCreate -> Bool
prop_end_within_step_of_create_time sc =
  fromIntegral (outputCEnd sc) > (fromIntegral (inputCTime sc) - (inputCStep sc) :: Int)

prop_last_value_equals_create_value :: SeriesCreate -> Bool
prop_last_value_equals_create_value sc =
  inputCVal sc == head (reverse (outputCValues sc))

prop_total_values_equals_create_value :: SeriesCreate -> Bool
prop_total_values_equals_create_value sc =
  inputCVal sc == sum (outputCValues sc)

prop_resolution_preserved_by_update :: SeriesUpdate -> Bool
prop_resolution_preserved_by_update su =
  inputURes su == outputURes su

prop_step_preserved_by_update :: SeriesUpdate -> Bool
prop_step_preserved_by_update su =
  inputUStep su == outputUStep su

prop_old_values_ignored_by_update :: SeriesUpdate -> Property
prop_old_values_ignored_by_update su =
  isUpdateBeforeStart su ==> inputUSeries su == outputUSeries su

prop_later_times_move_end_along_in_update :: SeriesUpdate -> Property
prop_later_times_move_end_along_in_update su =
  isUpdateAfterEnd su ==> inputUEnd su < outputUEnd su

prop_new_end_less_than_or_equal_to_update_time :: SeriesUpdate -> Property
prop_new_end_less_than_or_equal_to_update_time su =
  isUpdateAfterEnd su ==> fromIntegral (outputUEnd su) <= (fromIntegral (inputUTime su) :: Int)

prop_new_end_within_step_of_create_time :: SeriesUpdate -> Property
prop_new_end_within_step_of_create_time su =
  isUpdateAfterEnd su ==> fromIntegral (outputUEnd su) > (fromIntegral (inputUTime su) - (inputUStep su) :: Int)

prop_new_end_last_value_equals_update_value :: SeriesUpdate -> Property
prop_new_end_last_value_equals_update_value su =
  isUpdateAfterEnd su ==> inputUVal su == head (reverse (outputUValues su))

prop_update_between_start_and_end_adds_value :: SeriesUpdate -> Property
prop_update_between_start_and_end_adds_value su =
  isUpdateBetweenStartAndEnd su ==> prettyClose (sum (inputUValues su) + inputUVal su) (sum (outputUValues su))

prop_end_divisible_by_step :: Series -> Bool
prop_end_divisible_by_step series =
  0 == fromIntegral (end series) `mod` step series

prop_start_end_equals_resolution_step :: Series -> Bool
prop_start_end_equals_resolution_step series =
    fromIntegral (end series - start series) == (resolution series * step series)

prop_start_equals_end_resolution_step :: Series -> Bool
prop_start_equals_end_resolution_step series =
    start series == (end series - fromIntegral (resolution series * step series))

prop_values_length_equals_resolution :: Series -> Bool
prop_values_length_equals_resolution series =
   resolution series == length (values series)

prop_ordered_by_insertion_time :: Series -> Property
prop_ordered_by_insertion_time series =
    forAll (vector $ resolution series) $ \xs ->
        xs == values (foldl upd series xs)
  where
    upd s v = update (incr s) v s
    incr s  = fromIntegral (end s) + fromIntegral (step s)

data SeriesCreate = SeriesCreate {
    inputCRes :: Resolution
  , inputCStep :: Step
  , inputCTime :: Time
  , inputCVal :: Double
  , outputCSeries :: Series
  , outputCRes :: Resolution
  , outputCStep :: Step
  , outputCStart :: Interval
  , outputCEnd :: Interval
  , outputCValues :: [Double]
} deriving Show

instance Arbitrary SeriesCreate where
  arbitrary = do
    r <- choose (1, maxResolution)
    s <- choose (1, 1000)
    t <- arbitrary
    NonNegative v <- arbitrary
    let series = create r s t v
    return $ SeriesCreate {
      inputCRes = r
    , inputCStep = s
    , inputCTime = t
    , inputCVal = v
    , outputCSeries = series
    , outputCRes = resolution series
    , outputCStep = step series
    , outputCStart = start series
    , outputCEnd = end series
    , outputCValues = values series
    }

data SeriesUpdate = SeriesUpdate {
    inputUTime :: Time
  , inputUVal :: Double
  , inputUSeries :: Series
  , inputURes :: Resolution
  , inputUStep :: Step
  , inputUStart :: Interval
  , inputUEnd :: Interval
  , inputUValues :: [Double]
  , outputUSeries :: Series
  , outputURes :: Resolution
  , outputUStep :: Step
  , outputUStart :: Interval
  , outputUEnd :: Interval
  , outputUValues :: [Double]
} deriving Show

isUpdateBeforeStart :: SeriesUpdate -> Bool
isUpdateBeforeStart su =
  (fromIntegral (inputUTime su) :: Int) < fromIntegral (inputUStart su)

isUpdateAfterEnd :: SeriesUpdate -> Bool
isUpdateAfterEnd su =
  (fromIntegral (inputUTime su) :: Int) >= fromIntegral (inputUEnd su) + inputUStep su

isUpdateBetweenStartAndEnd :: SeriesUpdate -> Bool
isUpdateBetweenStartAndEnd su = 
  (fromIntegral (inputUTime su) :: Int) >= fromIntegral (inputUStart su)
   && (fromIntegral (inputUTime su) :: Int) < fromIntegral (inputUEnd su) + inputUStep su

instance Arbitrary SeriesUpdate where
  arbitrary = do
    s <- arbitrary
    NonNegative t <- arbitrary
    NonNegative v <- arbitrary
    let series = update t v s
    return $ SeriesUpdate {
      inputUTime = t
    , inputUVal = v
    , inputUSeries = s
    , inputURes = resolution s
    , inputUStep = step s
    , inputUStart = start s
    , inputUEnd = end s
    , inputUValues = values s
    , outputUSeries = series
    , outputURes = resolution s
    , outputUStep = step series
    , outputUStart = start series
    , outputUEnd = end series
    , outputUValues = values series
    }

instance Arbitrary Series where
  arbitrary = do
    s0 <- outputCSeries <$> arbitrary
    tvs <- arbitrary
    return $ foldl (\s (NonNegative t, NonNegative v) -> update t v s) s0 tvs

prettyClose :: (Num a, Fractional a, Ord a) => a -> a -> Bool
prettyClose a b | a > (b - 0.0001) && (a < b + 0.0001) = True
prettyClose _ _ = False
