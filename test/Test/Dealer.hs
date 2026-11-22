module Test.Dealer (tests) where

import Data.List qualified as List
import Data.Vec qualified as Vec
import Data.Vector.Unboxed qualified as VU
import Jasskell.Card ()
import Jasskell.Card qualified as Card
import Jasskell.Card.Internal ()
import Jasskell.Card.Set qualified as CardSet
import Jasskell.Dealer qualified as Dealer
import System.Random qualified as Random
import Test.Card ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Dealer"
    [ testGroup
        "shuffle"
        [ testProperty "has 36 cards" $
            \(Large seed) ->
              let (cards, _) = Dealer.shuffle CardSet.deck $ Random.mkStdGen seed
               in VU.length cards == 36,
          testProperty "contains full deck" $
            \(Large seed) ->
              let (cards, _) = Dealer.shuffle CardSet.deck $ Random.mkStdGen seed
               in VU.foldl' (flip CardSet.insert) CardSet.empty cards === CardSet.deck,
          testProperty "is deterministic" $
            \(Large seed) ->
              let gen = Random.mkStdGen seed
               in Dealer.shuffle CardSet.deck gen === Dealer.shuffle CardSet.deck gen,
          testProperty "uniform head" $
            checkCoverage $
              coverTable "rank" [(show @Card.Rank r, 10) | r <- [minBound .. maxBound]] $
                coverTable "suit" [(show @Card.Suit r, 24) | r <- [minBound .. maxBound]] $
                  \(Large seed) ->
                    let (cards, _) = Dealer.shuffle CardSet.deck $ Random.mkStdGen seed
                        c = VU.head cards
                     in tabulate "rank" [show $ Card.rank c] $
                          tabulate "suit" [show $ Card.suit c] True
        ],
      testGroup
        "four"
        [ testProperty "are distinct" $
            \(Large seed) ->
              let (hands, _) = Dealer.four $ Random.mkStdGen seed
               in conjoin $ do
                    h1 : hs <- List.tails $ Vec.toList hands
                    CardSet.null . CardSet.intersection h1 <$> hs,
          testProperty "are complete" $
            \(Large seed) ->
              let (hands, _) = Dealer.four $ Random.mkStdGen seed
               in Vec.foldl' CardSet.union CardSet.empty hands === CardSet.deck
        ]
    ]
