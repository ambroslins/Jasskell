module Main (main) where

import Test.Card qualified
import Test.CardSeq qualified
import Test.CardSet qualified
import Test.Dealer qualified
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
      Test.CardSeq.tests,
      Test.Dealer.tests,
      Test.GameState.tests
    ]
