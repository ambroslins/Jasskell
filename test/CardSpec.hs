module CardSpec where

import Card (Card (..), Rank (..), Suit (..))
import Card qualified
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
import Variant

spec :: Spec
spec = do
  describe "deck" $ do
    it "has 36 cards" $ length Card.deck `shouldBe` 36
  describe "value" $ do
    it "total deck value is 152" $
      forM_ allVariants $
        \var -> sum (map (Card.value var) $ toList Card.deck) `shouldBe` 152
  describe "compare" $ do
    prop "is transitive" $ \v l c1 c2 c3 ->
      let comp = Card.compare v l
       in comp c1 c2 == comp c2 c3 ==> comp c1 c3 === comp c1 c2
    prop "is reflexive" $ \v l c ->
      Card.compare v l c c === EQ
    prop "is antisymmetric" $ \v l c1 c2 ->
      let comp = Card.compare v l
       in c1 /= c2 ==> comp c1 c2 =/= comp c2 c1

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
