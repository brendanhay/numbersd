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

module Properties.Series (
      seriesProperties
    ) where

import Blaze.ByteString.Builder
import Control.Applicative                  ((<$>))
import Data.Maybe
import Numbers.Types
import Numbers.Whisper.Series
import Properties.Generators
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

seriesProperties :: Test.Framework.Test
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
  , testGroup "fetch" [
      testProperty "fetching from start to end is series identity" prop_fetch_start_to_end_is_series_identity
    , testProperty "fetching preserves the resolution" prop_fetch_preserves_resolution
    , testProperty "fetching preserves the step" prop_fetch_preserves_step
    , testProperty "fetched values are less than or equal to original" prop_fetch_values_less_than_or_equal_to_original
    ]
  , testGroup "series" [
      testProperty "end is divisible by step" prop_end_divisible_by_step
    , testProperty "start - end diff equals resolution * step" prop_start_end_equals_resolution_times_step
    , testProperty "values length equals resolution" prop_values_length_equals_resolution
    , testProperty "orders values by their insertion time" prop_ordered_by_insertion_time
    ]
  , testGroup "examples" [
      testCase "a worked example of a create" test_example_create
    , testCase "a worked example of an update" test_example_update
    , testCase "a worked example of a fetch" test_example_fetch
    ]
  ]

prop_input_resolution_used_by_create :: SeriesCreate -> Bool
prop_input_resolution_used_by_create sc =
  createInputRes sc == createOutputRes sc

prop_input_step_used_by_create :: SeriesCreate -> Bool
prop_input_step_used_by_create SeriesCreate{..} =
  createInputStep == createOutputStep

prop_end_less_than_or_equal_to_create_time :: SeriesCreate -> Bool
prop_end_less_than_or_equal_to_create_time SeriesCreate{..} =
  fromIntegral createOutputEnd <= (fromIntegral createInputTime :: Int)

prop_end_within_step_of_create_time :: SeriesCreate -> Bool
prop_end_within_step_of_create_time SeriesCreate{..} =
  fromIntegral createOutputEnd > (fromIntegral createInputTime - createInputStep :: Int)

prop_last_value_equals_create_value :: SeriesCreate -> Bool
prop_last_value_equals_create_value SeriesCreate{..} =
  createInputVal == fromJust (last createOutputValues)

prop_total_values_equals_create_value :: SeriesCreate -> Bool
prop_total_values_equals_create_value SeriesCreate{..} =
  createInputVal == sum (catMaybes createOutputValues)

prop_resolution_preserved_by_update :: SeriesUpdate -> Bool
prop_resolution_preserved_by_update SeriesUpdate{..} =
  updateInputRes == updateOutputRes

prop_step_preserved_by_update :: SeriesUpdate -> Bool
prop_step_preserved_by_update SeriesUpdate{..} =
  updateInputStep == updateOutputStep

prop_old_values_ignored_by_update :: SeriesUpdate -> Property
prop_old_values_ignored_by_update su@SeriesUpdate{..} =
  isUpdateBeforeStart su ==> updateInputSeries == updateOutputSeries

prop_later_times_move_end_along_in_update :: SeriesUpdate -> Property
prop_later_times_move_end_along_in_update su@SeriesUpdate{..} =
  isUpdateAfterEnd su ==> updateInputEnd < updateOutputEnd

prop_new_end_less_than_or_equal_to_update_time :: SeriesUpdate -> Property
prop_new_end_less_than_or_equal_to_update_time su@SeriesUpdate{..} =
  isUpdateAfterEnd su ==> fromIntegral updateOutputEnd <= (fromIntegral updateInputTime :: Int)

prop_new_end_within_step_of_create_time :: SeriesUpdate -> Property
prop_new_end_within_step_of_create_time su@SeriesUpdate{..} =
  isUpdateAfterEnd su ==> fromIntegral updateOutputEnd > (fromIntegral updateInputTime - updateInputStep :: Int)

prop_new_end_last_value_equals_update_value :: SeriesUpdate -> Property
prop_new_end_last_value_equals_update_value su@SeriesUpdate{..} =
  isUpdateAfterEnd su ==> updateInputVal == fromJust (last updateOutputValues)

prop_update_between_start_and_end_adds_value :: SeriesUpdate -> Property
prop_update_between_start_and_end_adds_value su@SeriesUpdate{..} =
  isUpdateBetweenStartAndEnd su ==> prettyClose (sum (catMaybes updateInputValues) + updateInputVal) (sum (catMaybes updateOutputValues))

prop_fetch_start_to_end_is_series_identity :: Series -> Bool
prop_fetch_start_to_end_is_series_identity series =
  series == fetch (Time . fromIntegral $ start series) (Time . fromIntegral $ end series) series

prop_fetch_preserves_resolution :: SeriesFetch -> Bool
prop_fetch_preserves_resolution SeriesFetch{..} =
  fetchInputRes == fetchOutputRes

prop_fetch_preserves_step :: SeriesFetch -> Bool
prop_fetch_preserves_step SeriesFetch{..} =
  fetchInputStep == fetchOutputStep

prop_fetch_values_less_than_or_equal_to_original :: SeriesFetch -> Bool
prop_fetch_values_less_than_or_equal_to_original SeriesFetch{..} =
  sum (catMaybes fetchOutputValues) <= sum (catMaybes fetchInputValues)

prop_end_divisible_by_step :: Series -> Bool
prop_end_divisible_by_step series =
  0 == fromIntegral (end series) `mod` step series

prop_start_end_equals_resolution_times_step :: Series -> Bool
prop_start_end_equals_resolution_times_step series =
    fromIntegral (end series - start series) == (resolution series * step series)

prop_values_length_equals_resolution :: Series -> Bool
prop_values_length_equals_resolution series =
   resolution series == length (values series)

prop_ordered_by_insertion_time :: Series -> Property
prop_ordered_by_insertion_time series =
    forAll (vector $ resolution series) $ \xs ->
        (map Just xs) == values (foldl upd series xs)
  where
    upd s v = update (incr s) v s
    incr s  = fromIntegral (end s) + fromIntegral (step s)


test_example_create :: Assertion
test_example_create = do
  let series = create 5 10 (Time 50000) 3.4
  assertEqual "resolution" 5 (resolution series)
  assertEqual "step" 10 (step series)
  assertEqual "end" (I 50000) (end series)
  assertEqual "start" (I 49950) (start series)
  assertEqual "values"
              [Nothing, Nothing, Nothing, Nothing, Just 3.4]
              (values series)
  assertEqual "build"
              "49950,50000,10|None,None,None,None,3.4"
              (toByteString $ build series)

test_example_update :: Assertion
test_example_update = do
  let series = update 50010 4.5 $ create 5 10 (Time 50000) 3.4
  assertEqual "resolution" 5 (resolution series)
  assertEqual "step" 10 (step series)
  assertEqual "end" (I 50010) (end series)
  assertEqual "start" (I 49960) (start series)
  assertEqual "values"
              [Nothing, Nothing, Nothing, Just 3.4, Just 4.5]
              (values series)
  assertEqual "build"
              "49960,50010,10|None,None,None,3.4,4.5"
              (toByteString $ build series)

test_example_fetch :: Assertion
test_example_fetch = do
  let series = fetch (Time 49950) (Time 50020)
                 . update 50010 4.5 $ create 5 10 (Time 50000) 3.4
  assertEqual "resolution" 5 (resolution series)
  assertEqual "step" 10 (step series)
  assertEqual "end" (I 50020) (end series)
  assertEqual "start" (I 49970) (start series)
  assertEqual "values"
              [Nothing, Nothing, Just 3.4, Just 4.5, Nothing]
              (values series)
  assertEqual "build"
              "49970,50020,10|None,None,3.4,4.5,None"
              (toByteString $ build series)

data SeriesCreate = SeriesCreate {
    createInputRes :: Resolution
  , createInputStep :: Step
  , createInputTime :: Time
  , createInputVal :: Double
  , createOutputSeries :: Series
  , createOutputRes :: Resolution
  , createOutputStep :: Step
  , createOutputStart :: Interval
  , createOutputEnd :: Interval
  , createOutputValues :: [Maybe Double]
} deriving Show

instance Arbitrary SeriesCreate where
  arbitrary = do
    r <- choose (1, maxResolution)
    s <- choose (1, 1000)
    t <- arbitrary
    NonNegative v <- arbitrary
    let series = create r s t v
    return SeriesCreate {
      createInputRes = r
    , createInputStep = s
    , createInputTime = t
    , createInputVal = v
    , createOutputSeries = series
    , createOutputRes = resolution series
    , createOutputStep = step series
    , createOutputStart = start series
    , createOutputEnd = end series
    , createOutputValues = values series
    }

data SeriesUpdate = SeriesUpdate {
    updateInputTime :: Time
  , updateInputVal :: Double
  , updateInputSeries :: Series
  , updateInputRes :: Resolution
  , updateInputStep :: Step
  , updateInputStart :: Interval
  , updateInputEnd :: Interval
  , updateInputValues :: [Maybe Double]
  , updateOutputSeries :: Series
  , updateOutputRes :: Resolution
  , updateOutputStep :: Step
  , updateOutputStart :: Interval
  , updateOutputEnd :: Interval
  , updateOutputValues :: [Maybe Double]
} deriving Show

isUpdateBeforeStart :: SeriesUpdate -> Bool
isUpdateBeforeStart SeriesUpdate{..} =
  (fromIntegral updateInputTime :: Int) < fromIntegral updateInputStart + updateInputStep

isUpdateAfterEnd :: SeriesUpdate -> Bool
isUpdateAfterEnd SeriesUpdate{..} =
  (fromIntegral updateInputTime :: Int) >= fromIntegral updateInputEnd + updateInputStep

isUpdateBetweenStartAndEnd :: SeriesUpdate -> Bool
isUpdateBetweenStartAndEnd su =
  not (isUpdateBeforeStart su) && not (isUpdateAfterEnd su)

instance Arbitrary SeriesUpdate where
  arbitrary = do
    s <- arbitrary
    NonNegative t <- arbitrary
    NonNegative v <- arbitrary
    let series = update t v s
    return SeriesUpdate {
      updateInputTime = t
    , updateInputVal = v
    , updateInputSeries = s
    , updateInputRes = resolution s
    , updateInputStep = step s
    , updateInputStart = start s
    , updateInputEnd = end s
    , updateInputValues = values s
    , updateOutputSeries = series
    , updateOutputRes = resolution s
    , updateOutputStep = step series
    , updateOutputStart = start series
    , updateOutputEnd = end series
    , updateOutputValues = values series
    }

data SeriesFetch = SeriesFetch {
    fetchInputFrom :: Time
  , fetchInputTo :: Time
  , fetchInputSeries :: Series
  , fetchInputRes :: Resolution
  , fetchInputStep :: Step
  , fetchInputStart :: Interval
  , fetchInputEnd :: Interval
  , fetchInputValues :: [Maybe Double]
  , fetchOutputSeries :: Series
  , fetchOutputRes :: Resolution
  , fetchOutputStep :: Step
  , fetchOutputStart :: Interval
  , fetchOutputEnd :: Interval
  , fetchOutputValues :: [Maybe Double]
} deriving Show

instance Arbitrary SeriesFetch where
  arbitrary = do
    s <- arbitrary
    NonNegative f <- arbitrary
    NonNegative t <- arbitrary
    let series = fetch f t s
    return SeriesFetch {
      fetchInputFrom = f
    , fetchInputTo = t
    , fetchInputSeries = s
    , fetchInputRes = resolution s
    , fetchInputStep = step s
    , fetchInputStart = start s
    , fetchInputEnd = end s
    , fetchInputValues = values s
    , fetchOutputSeries = series
    , fetchOutputRes = resolution s
    , fetchOutputStep = step series
    , fetchOutputStart = start series
    , fetchOutputEnd = end series
    , fetchOutputValues = values series
    }

instance Arbitrary Series where
  arbitrary = do
    s <- createOutputSeries <$> arbitrary
    actions <- arbitrary
    return $ foldl applyAction s actions
    where
      applyAction :: Series -> Either (NonNegative Time, NonNegative Time) (NonNegative Time, NonNegative Double) -> Series
      applyAction s (Left (NonNegative f, NonNegative t)) = fetch f t s
      applyAction s (Right (NonNegative t, NonNegative v)) = update t v s
