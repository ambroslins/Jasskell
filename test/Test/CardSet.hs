module Test.CardSet (tests) where

import Jasskell.Card qualified as Card
import Jasskell.Card.Set (CardSet)
import Jasskell.Card.Set qualified as CardSet
import Test.Card ()
-- orphan instances
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "CardSeq"
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
        ]
    ]

instance Arbitrary CardSet where
  arbitrary = CardSet.fromList <$> arbitrary
  shrink = fmap CardSet.fromList . shrink . CardSet.toList
