module CardTest where

import Gen qualified
import Hedgehog (Property, forAll, property, withDiscards, (/==), (===))
import Jasskell.Card (Card (..), Rank (..), Suit (..))
import Jasskell.Card qualified as Card
import Jasskell.Variant (Direction (..), Variant (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Card"
    [ testGroup
        "deck"
        [ testCase "has 36 cards" $ length Card.deck @?= 36
        ],
      testGroup
        "value"
        [ testCase "returns 20 for puur" $
            Card.value (Trump Bells) (Card Bells Under) @?= 20,
          testCase "returns 14 for nell" $
            Card.value (Trump Bells) (Card Bells Nine)
              @?= 14,
          testProperty "totals 152 for all cards" prop_value_totals_152
        ],
      testGroup
        "compare"
        [ testProperty "is transitive" prop_compare_is_transitive,
          testProperty "is reflexiv" prop_compare_is_reflexive,
          testProperty "is antisymetric" prop_compare_is_antisymmertric,
          testProperty
            "on variant 'Trump' matches 'TopDown' if no card is trump"
            prop_compare_trump_match_topdown,
          testGroup
            "when variant is 'Trump Bells' and 'Hearts' lead"
            $ let testIsGT c1 c2 =
                    testCase (show c1 <> " is greater than " <> show c2) $
                      Card.compare (Trump Bells) Hearts c1 c2 @?= GT
               in [ testIsGT (Card Bells Under) (Card Bells King),
                    testIsGT (Card Bells Under) (Card Bells Nine),
                    testIsGT (Card Bells Nine) (Card Bells Ace),
                    testIsGT (Card Bells Over) (Card Bells Eight),
                    testIsGT (Card Bells Seven) (Card Hearts King),
                    testIsGT (Card Hearts Ten) (Card Hearts Nine),
                    testIsGT (Card Hearts Eight) (Card Acorns Ace)
                  ],
          testGroup
            "when variant is 'Direction BottomUp' and 'Hearts' lead"
            $ let testIsGT c1 c2 =
                    testCase (show c1 <> " is greater than " <> show c2) $
                      Card.compare (Direction BottomUp) Hearts c1 c2 @?= GT
               in [ testIsGT (Card Hearts Seven) (Card Hearts Nine),
                    testIsGT (Card Hearts Nine) (Card Hearts King),
                    testIsGT (Card Hearts Over) (Card Bells Six)
                  ]
        ]
    ]

prop_value_totals_152 :: Property
prop_value_totals_152 = property $ do
  variant <- forAll Gen.variant
  sum ((map $ Card.value variant) (toList Card.deck)) === 152

prop_compare_is_transitive :: Property
prop_compare_is_transitive = withDiscards 200 $
  property $ do
    v <- forAll Gen.variant
    l <- forAll Gen.suit
    c1 <- forAll Gen.card
    c2 <- forAll Gen.card
    c3 <- forAll Gen.card
    let comp = Card.compare v l
    guard $ comp c1 c2 == comp c2 c3
    comp c1 c3 === comp c1 c2

prop_compare_is_reflexive :: Property
prop_compare_is_reflexive = property $ do
  v <- forAll Gen.variant
  l <- forAll Gen.suit
  c1 <- forAll Gen.card
  c2 <- forAll Gen.card
  (Card.compare v l c1 c2 == EQ) === (c1 == c2)

prop_compare_is_antisymmertric :: Property
prop_compare_is_antisymmertric = property $ do
  v <- forAll Gen.variant
  l <- forAll Gen.suit
  c1 <- forAll Gen.card
  c2 <- forAll Gen.card
  guard $ c1 /= c2
  let comp = Card.compare v l
  comp c1 c2 /== comp c2 c1

prop_compare_trump_match_topdown :: Property
prop_compare_trump_match_topdown = property $ do
  t <- forAll Gen.suit
  l <- forAll Gen.suit
  c1 <- forAll Gen.card
  c2 <- forAll Gen.card
  guard $ Card.suit c1 /= t
  guard $ Card.suit c2 /= t
  Card.compare (Trump t) l c1 c2 === Card.compare (Direction TopDown) l c1 c2