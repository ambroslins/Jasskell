module Main where

import CardTest qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import VectorTest qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ CardTest.tests,
      VectorTest.tests
    ]
