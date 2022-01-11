module CardSpec where

import Card (Card (..), Rank (..), Suit (..))
import Card qualified
import Data.Set qualified as Set
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
  ( conjoin,
    counterexample,
    discard,
    disjoin,
    elements,
    label,
    (=/=),
    (===),
    (==>),
  )
import Test.QuickCheck.Arbitrary
  ( Arbitrary (arbitrary),
    arbitraryBoundedEnum,
  )
import Test.QuickCheck.Modifiers (NonEmptyList (..))
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
  describe "status" $ do
    prop "some hand card is playable" $ \v t (NonEmpty h) ->
      disjoin $ map (\c -> Card.status v t (Set.fromList h) c == Card.Playable) h
    prop "checks not in hand" $ \v t h c ->
      Set.notMember c h ==> Card.status v t h c === Card.NotInHand
    prop "holds invariants" $ \v t h c ->
      case Card.status v t h c of
        Card.Playable ->
          label "Playable" $ Set.member c h
        Card.FollowTrump trump ->
          label "FollowTrump" $
            conjoin
              [ v === Trump trump,
                suit c =/= trump,
                fmap (suit . head) (nonEmpty t) === Just trump
              ]
        Card.FollowLead lead ->
          label "FollowLead" $
            conjoin
              [ suit c =/= lead,
                fmap (suit . head) (nonEmpty t) === Just lead
              ]
        Card.Undertrump highest ->
          label "Undertrump" $
            conjoin
              [ counterexample (show (highest, t)) $ highest `elem` t,
                v === Trump (suit highest),
                fmap (suit . head) (nonEmpty t) =/= Just (suit highest)
              ]
        Card.NotInHand -> discard

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
