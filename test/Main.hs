module Main (main) where

import Test.Card qualified
import Test.Card qualified as Test.CardSet
import Test.GameState qualified
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ Test.Card.tests,
      Test.CardSet.tests,
      Test.GameState.tests
    ]
