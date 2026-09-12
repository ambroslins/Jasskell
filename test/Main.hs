module Main (main) where

import Test.Card qualified
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Test.Card.tests
    ]
