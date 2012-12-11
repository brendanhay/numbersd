{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Properties.Map
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Properties.Map (
      mapProperties
    ) where

import Control.Applicative                  ((<$>))
import Control.Concurrent.Async
import Control.Monad
import Data.List
import Data.Ord
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Numbers.Map as M


mapProperties :: Test
mapProperties = testGroup "map"
  [ testGroup "empty" [
      testProperty "no values in an empty map" prop_no_vals_in_empty_map
    , testProperty "no policy map is just a map" prop_no_policy_map_is_just_map
    ]
  ]

prop_no_vals_in_empty_map :: Property
prop_no_vals_in_empty_map = monadicIO $ do
  m <- emptyNoPolicyMap
  assertMapContains (TestData []) m

prop_no_policy_map_is_just_map :: TestData -> Property
prop_no_policy_map_is_just_map xs = monadicIO $ do
  m <- emptyNoPolicyMap
  _ <- addAll xs m
  assertMapContains (sumUp xs) m

sumUp :: TestData -> TestData
sumUp (TestData xs) = TestData . map (\kvs -> (fst $ head kvs, TestValue . sum $ map (val . snd) kvs)) . groupBy (\a b -> fst a == fst b) $ sortBy (comparing fst) xs

addAll :: TestData -> M.Map TestKey TestValue -> PropertyM IO ()
addAll (TestData xs) m = run . void $ mapConcurrently (flip add m) xs
  where
    add :: (TestKey, TestValue) -> M.Map TestKey TestValue -> IO ()
    add (k,TestValue v) = M.update k (return . maybe (TestValue v) (\(TestValue v') -> TestValue (v + v')))

emptyNoPolicyMap :: PropertyM IO (M.Map TestKey TestValue)
emptyNoPolicyMap = run $ M.empty M.NoPolicy

assertMapContains :: TestData -> M.Map TestKey TestValue -> PropertyM IO ()
assertMapContains (TestData expected) m = do
  actual <- run $ M.toList m
  actualkeys <- run $ M.keys m
  assertEqual "keys" (map fst expected) actualkeys
  mapM_ (\(k,v) -> do v' <- run $ M.lookup k m; assertEqual ("lookup " ++ show k) (Just v)  v') expected
  assertEqual "toList" expected actual

val :: TestValue -> Int
val (TestValue v) = v

data TestData = TestData [(TestKey, TestValue)]
  deriving Show

data TestKey = TestKey Char
  deriving (Eq, Ord, Show)

data TestValue = TestValue Int
  deriving (Eq, Ord, Show)

instance Arbitrary TestData where
  arbitrary = do
    n <- oneof $ map return [50..100]
    TestData <$> replicateM n arbitrary

instance Arbitrary TestKey where
  arbitrary = TestKey <$> choose ('A', 'Z')

instance Arbitrary TestValue where
  arbitrary = TestValue <$> choose (1, 30)

assertEqual :: (Eq a, Show a) => String -> a -> a -> PropertyM IO ()
assertEqual _ a b | a == b = assert True
assertEqual msg a b = do
  run . putStrLn $ msg ++ " assertEqual failed.."
  run . putStrLn $ "Expected: " ++ show a
  run . putStrLn $ "Actual  : " ++ show b
  assert False