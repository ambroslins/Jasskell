module CardSpec where

import Jasskell.Card
  ( Card (..),
    Rank (..),
    Suit (..),
  )
import Jasskell.Card qualified as Card
import Jasskell.Variant (Direction (..), Variant (..))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
  ( elements,
    (=/=),
    (===),
    (==>),
  )
import Test.QuickCheck.Arbitrary
  ( Arbitrary (arbitrary),
    arbitraryBoundedEnum,
  )

spec :: Spec
spec = do
  describe "deck" $ do
    it "has 36 cards" $ length Card.deck `shouldBe` 36

  describe "value" $ do
    it "totals 152 for all cards" $
      forM_ allVariants $
        \var -> sum (map (Card.value var) $ toList Card.deck) `shouldBe` 152
    it "returns 20 for puur" $
      Card.value (Trump Bells) (Card Bells Under) `shouldBe` 20
    it "returns 14 for nell" $
      Card.value (Trump Bells) (Card Bells Nine) `shouldBe` 14

  describe "compare" $ do
    prop "is transitive" $ \v l c1 c2 c3 ->
      let comp = Card.compare v l
       in comp c1 c2 == comp c2 c3 ==> comp c1 c3 === comp c1 c2
    prop "is reflexive" $ \v l c1 c2 ->
      (Card.compare v l c1 c2 == EQ) === (c1 == c2)
    prop "is antisymmetric" $ \v l c1 c2 ->
      let comp = Card.compare v l
       in c1 /= c2 ==> comp c1 c2 =/= comp c2 c1
    prop "on variant 'Trump' matches 'TopDown' if no card is trump" $
      \t l c1 c2 ->
        t `notElem` map suit [c1, c2]
          ==> Card.compare (Trump t) l c1 c2 === Card.compare (Direction TopDown) l c1 c2
    prop "on variants 'Direction' and 'Slalom' match" $
      \d l c1 c2 ->
        Card.compare (Direction d) l c1 c2 === Card.compare (Slalom d) l c1 c2
    let mapIsGT variant lead = mapM_ $
          \(c1, c2) ->
            it (show c1 <> " is greater than " <> show c2) $
              Card.compare variant lead c1 c2 `shouldBe` GT
    describe "when variant is 'Trump Bells' and 'Hearts' lead" $
      mapIsGT
        (Trump Bells)
        Hearts
        [ (Card Bells Over, Card Bells Eight),
          (Card Bells Nine, Card Bells Ace),
          (Card Bells Under, Card Bells Nine),
          (Card Bells Seven, Card Hearts King),
          (Card Hearts King, Card Hearts Nine),
          (Card Hearts Eight, Card Acorns Ace)
        ]
    describe "when variant is 'Direction TopDown' and 'Bells' lead" $
      mapIsGT
        (Direction TopDown)
        Bells
        [ (Card Bells Over, Card Bells Eight),
          (Card Bells Ace, Card Bells Nine),
          (Card Bells King, Card Bells Under),
          (Card Bells Seven, Card Hearts King)
        ]
    describe "when variant is 'Direction BottomUp' and 'Bells' lead" $
      mapIsGT
        (Direction BottomUp)
        Bells
        [ (Card Bells Eight, Card Bells Over),
          (Card Bells Nine, Card Bells King),
          (Card Bells Under, Card Bells Ace),
          (Card Bells King, Card Hearts Seven)
        ]

allVariants :: [Variant]
allVariants =
  (Trump <$> universe)
    ++ ([Direction, Slalom] <*> [TopDown, BottomUp])

instance Arbitrary Rank where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Suit where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Card where
  arbitrary = Card <$> arbitrary <*> arbitrary

instance Arbitrary Direction where
  arbitrary = elements [TopDown, BottomUp]

instance Arbitrary Variant where
  arbitrary = elements allVariants
