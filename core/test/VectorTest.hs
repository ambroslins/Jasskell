module VectorTest (tests) where

import Control.Monad.Writer (MonadWriter (tell), runWriter)
import Data.List qualified as List
import Data.Typeable ((:~:) (Refl))
import Data.Vector.Sized qualified as Vector
import Data.Vector.Sized.Extra qualified as Vector
import Gen qualified
import Hedgehog (Property, evalMaybe, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Vector.Extra"
    [ testGroup
        "rotate"
        [ testProperty "with 0 is identity" prop_rotate_zero,
          testProperty "has an inverse" prop_rotate_has_inverse,
          testProperty "has the same elements" prop_rotate_elements
        ],
      testGroup
        "notEmpty"
        [ testCase "returns `Nothing` on empty vector" $
            Vector.notEmpty (Vector.empty @Int) @?= Nothing,
          testProperty "returns `Just Refl` on non empty vector" prop_notEmpty
        ],
      testGroup
        "unfoldrM"
        [ testProperty "matches the reference" prop_unfoldrM_ref
        ],
      testGroup
        "iterateM"
        [ testProperty "matches the reference" prop_iterateM_ref
        ],
      testGroup
        "constructM"
        [ testProperty "matches the reference" prop_constructM_ref
        ]
    ]

prop_rotate_zero :: Property
prop_rotate_zero = property $ do
  list <- forAll $ Gen.list (Range.linear 0 100) $ Gen.int Range.linearBounded
  Vector.withSizedList list $ \vec -> do
    Vector.rotate 0 vec === vec

prop_rotate_has_inverse :: Property
prop_rotate_has_inverse = property $ do
  list <- forAll $ Gen.list (Range.linear 1 100) $ Gen.int Range.linearBounded
  Vector.withSizedList list $ \vec -> do
    n <- forAll Gen.finite
    Vector.rotate (negate n) (Vector.rotate n vec) === vec

prop_rotate_elements :: Property
prop_rotate_elements = property $ do
  list <- forAll $ Gen.list (Range.linear 1 100) $ Gen.int Range.linearBounded
  Vector.withSizedList list $ \vec -> do
    n <- forAll Gen.finite
    let sortVec = List.sort . Vector.toList
    sortVec (Vector.rotate n vec) === sortVec vec

prop_notEmpty :: Property
prop_notEmpty = property $ do
  list <- forAll $ Gen.list (Range.linear 1 100) $ Gen.int Range.linearBounded
  Vector.withSizedList list $ \vec -> do
    Refl <- evalMaybe $ Vector.notEmpty vec
    pure ()

prop_unfoldrM_ref :: Property
prop_unfoldrM_ref = property $ do
  n <- forAll $ Gen.integral (Range.linear 0 16)
  let f y = tell [y `mod` n] $> (y * y, (y + 1) * 2)
  case someNatVal n of
    SomeNat (Proxy :: Proxy n) -> do
      let (vec, result) = runWriter (Vector.unfoldrM @n f 0)
          (list, expected) = runWriter (unfoldrM n f 0)
      Vector.toList vec === list
      result === expected

prop_iterateM_ref :: Property
prop_iterateM_ref = property $ do
  n <- forAll $ Gen.integral (Range.linear 0 16)
  let f y = tell [y `mod` n] $> (y + 1) * 2
  case someNatVal n of
    SomeNat (Proxy :: Proxy n) -> do
      let (vec, result) = runWriter (Vector.iterateM @n f 0)
          (list, expected) = runWriter (iterateM n f 0)
      Vector.toList vec === list
      result === expected

prop_constructM_ref :: Property
prop_constructM_ref = property $ do
  n <- forAll $ Gen.integral (Range.linear 0 16)
  let f xs = tell [sum xs] $> length xs
  case someNatVal n of
    SomeNat (Proxy :: Proxy n) -> do
      let (vec, result) = runWriter (Vector.constructM @n f)
          (list, expected) = runWriter (constructM n f)
      Vector.toList vec === list
      result === expected

-- Reference implementations for lists

unfoldrM :: Monad m => Natural -> (b -> m (a, b)) -> b -> m [a]
unfoldrM n f x
  | n == 0 = pure []
  | otherwise = do
    (y, z) <- f x
    (y :) <$> unfoldrM (n - 1) f z

iterateM :: Monad m => Natural -> (a -> m a) -> a -> m [a]
iterateM n f x
  | n == 0 = pure []
  | n == 1 = pure [x]
  | otherwise = (x :) <$> (f x >>= iterateM (n - 1) f)

constructM :: Monad m => Natural -> ([a] -> m a) -> m [a]
constructM n f = go n []
  where
    go i xs
      | i == 0 = pure xs
      | otherwise = f xs >>= \x -> go (i - 1) (xs ++ [x])