module Test.CardSet (tests) where

import Data.Vector.Unboxed qualified as VU
import Jasskell.Card ()
import Jasskell.Card qualified as Card
import Jasskell.Card.Set (CardSet)
import Jasskell.Card.Set qualified as CardSet
import System.Random qualified as Random
import Test.Card ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "CardSet"
    [ testGroup
        "fromList"
        [ testProperty "all cards are members" $
            \cs -> let set = CardSet.fromList cs in all (`CardSet.member` set) cs,
          testProperty "inverse of toList" $
            \cs -> cs === CardSet.fromList (CardSet.toList cs)
        ],
      testGroup
        "filterSuit"
        [ testProperty "equals filter on suit" $
            \s cs -> CardSet.filterSuit s cs === CardSet.filter (\c -> Card.suit c == s) cs,
          testProperty "all suit" $
            \s cs -> all (\c -> Card.suit c == s) . CardSet.toList $ CardSet.filterSuit s cs
        ],
      testGroup
        "shuffle"
        [ testProperty "has 36 cards" $
            \(Large seed) ->
              let (cards, _) = CardSet.shuffle $ Random.mkStdGen seed
               in VU.length cards == 36,
          testProperty "contains full deck" $
            \(Large seed) ->
              let (cards, _) = CardSet.shuffle $ Random.mkStdGen seed
               in VU.foldl' (flip CardSet.insert) CardSet.empty cards === CardSet.deck,
          testProperty "is deterministic" $
            \(Large seed) ->
              let gen = Random.mkStdGen seed
               in CardSet.shuffle gen === CardSet.shuffle gen,
          testProperty "uniform head" $
            checkCoverage $
              coverTable "rank" [(show @Card.Rank r, 10) | r <- [minBound .. maxBound]] $
                coverTable "suit" [(show @Card.Suit r, 24) | r <- [minBound .. maxBound]] $
                  \(Large seed) ->
                    let (cards, _) = CardSet.shuffle $ Random.mkStdGen seed
                        c = VU.head cards
                     in tabulate "rank" [show $ Card.rank c] $
                          tabulate "suit" [show $ Card.suit c] True
        ]
    ]

instance Arbitrary CardSet where
  arbitrary = CardSet.fromList <$> arbitrary
  shrink = fmap CardSet.fromList . shrink . CardSet.toList
