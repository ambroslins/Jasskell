module Test.Card (tests) where

import Data.Text qualified as Text
import Jasskell.Card (Card, CardSet, Rank (..), Suit (..))
import Jasskell.Card qualified as Card
import Jasskell.Variant (Direction (..), Variant (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Card"
    [ testGroup
        "make"
        [ testProperty "has suit" $
            \s r -> let c = Card.make s r in Card.suit c === s,
          testProperty "has rank" $
            \s r -> let c = Card.make s r in Card.rank c === r
        ],
      testGroup
        "points"
        [ testProperty "sum is 152" $
            \var -> sum (map (Card.points var) $ Card.toList Card.deck) === 152
        ],
      testGroup
        "compare"
        [ testProperty "is transitive" $
            \var lead c1 c2 c3 ->
              let comp = Card.compare var lead
               in comp c1 c2 == comp c2 c3 ==> comp c1 c3 === comp c1 c2,
          testProperty "is reflexive" $
            \var lead c -> Card.compare var lead c c == EQ,
          testProperty "is anti-commutative" $
            \var lead c1 c2 ->
              let comp = Card.compare var lead
                  inv = \case LT -> GT; EQ -> EQ; GT -> LT
               in comp c1 c2
                    === inv
                      (comp c2 c1),
          testGroup
            "when trump is 'Bells' and 'Hearts' lead"
            $ let testIsGT c1 c2 =
                    testCase (Text.unpack $ Card.abbreviation c1 <> " is greater than " <> Card.abbreviation c2) $
                      Card.compare Hearts (Trump Bells) c1 c2 @?= GT
               in [ testIsGT (Card.make Bells Under) (Card.make Bells King),
                    testIsGT (Card.make Bells Under) (Card.make Bells Nine),
                    testIsGT (Card.make Bells Nine) (Card.make Bells Ace),
                    testIsGT (Card.make Bells Over) (Card.make Bells Eight),
                    testIsGT (Card.make Bells Seven) (Card.make Hearts King),
                    testIsGT (Card.make Hearts Ten) (Card.make Hearts Nine),
                    testIsGT (Card.make Hearts Eight) (Card.make Acorns Ace)
                  ]
        ],
      testGroup
        "fromList"
        [ testProperty "all cards are members" $
            \cs -> let set = Card.fromList cs in all (`Card.member` set) cs,
          testProperty "inverse of toList" $
            \cs -> cs === Card.fromList (Card.toList cs)
        ],
      testGroup
        "filterSuit"
        [ testProperty "equals filter on suit" $
            \s cs -> Card.filterSuit s cs === Card.filter (\c -> Card.suit c == s) cs,
          testProperty "all suit" $
            \s cs -> all (\c -> Card.suit c == s) . Card.toList $ Card.filterSuit s cs
        ]
    ]

instance Arbitrary Rank where
  arbitrary = chooseEnum (minBound, maxBound)

instance Arbitrary Suit where
  arbitrary = chooseEnum (minBound, maxBound)

instance Arbitrary Card where
  arbitrary = Card.make <$> arbitrary <*> arbitrary

instance Arbitrary CardSet where
  arbitrary = Card.fromList <$> arbitrary
  shrink = fmap Card.fromList . shrink . Card.toList

instance Arbitrary Variant where
  arbitrary =
    oneof
      [ Trump <$> arbitrary,
        elements [Direction, Slalom] <*> elements [BottomUp, TopDown]
      ]
