module Test.GameState (tests) where

import Data.List qualified as List
import Jasskell.Card qualified as Card
import Jasskell.GameState qualified as GameState
import System.Random qualified as Random
import Test.QuickCheck.Modifiers (NoShrink (..))
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "GameState"
    [ testGroup
        "new"
        [ testProperty "currentPlayer has weli" $
            \(NoShrink seed) ->
              let gs = GameState.new $ Random.mkStdGen64 seed
                  current = GameState.currentPlayer gs
                  view = GameState.viewFor current gs
               in Card.weli `Card.member` view.hand,
          testProperty "disjoint hands" $
            \(NoShrink seed) ->
              let gs = GameState.new $ Random.mkStdGen64 seed
                  views = map (`GameState.viewFor` gs) [0 .. 3]
               in conjoin
                    [ Card.null $ v1.hand `Card.intersection` v2.hand
                    | v1 : vs <- List.inits views,
                      v2 <- vs
                    ]
        ]
    ]
