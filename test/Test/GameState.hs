module Test.GameState (tests) where

import Test.Tasty
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "GameState"
    [testCase "placeholder" (pure ())]
