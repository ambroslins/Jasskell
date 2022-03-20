module CardSpec where

import Card
  ( Card (..),
    Rank (..),
    Suit (..),
    suit,
  )
import Card qualified
import List qualified
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
    it "totals 152 for all cards" $
      forM_ allVariants $
        \var -> sum (List.map (Card.value var) $ toList Card.deck) `shouldBe` 152
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
        t `List.notElem` List.map suit [c1, c2]
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

{-
  describe "status" $ do
    prop "returns 'Playable' for some hand card" $ \v t (NonEmpty h) ->
      disjoin $ List.map (\c -> Card.status v t (Set.fromList h) c == Card.Playable) h
    prop "returns 'NotInHand' if the card is not in the hand" $ \v t h c ->
      Set.notMember c h ==> Card.status v t h c === Unplayable NotInHand
    prop "returns 'Playable' for a singleton hand" $ \v t c ->
      Card.status v t (Set.singleton c) c === Card.Playable
    prop "holds some invariants" $ \v t h c ->
      case Card.status v t h c of
        Playable ->
          label "Playable" $ Set.member c h
        Unplayable reason -> case reason of
          FollowTrump trump ->
            label "FollowTrump" $
              conjoin
                [ v === Trump trump,
                  suit c =/= trump,
                  fmap (suit . head) (nonEmpty t) === Just trump
                ]
          FollowLead lead ->
            label "FollowLead" $
              conjoin
                [ suit c =/= lead,
                  fmap (suit . head) (nonEmpty t) === Just lead
                ]
          Undertrump highest ->
            label "Undertrump" $
              conjoin
                [ counterexample (show (highest, t)) $ highest `List.elem` t,
                  v === Trump (suit highest),
                  fmap (suit . head) (nonEmpty t) =/= Just (suit highest)
                ]
          NotInHand -> discard
    it "returns 'Playable' for a playable Card" $
      Card.status
        (Trump Bells)
        [Card Hearts Over, Card Acorns Seven]
        (Set.fromList [Card Bells Ten, Card Hearts King, Card Leaves Nine])
        (Card Hearts King)
        `shouldBe` Playable
    it "returns 'FollowTrump' if card doesn't follow the trump" $
      Card.status
        (Trump Bells)
        [Card Bells Over, Card Acorns Seven]
        (Set.fromList [Card Bells Ten, Card Hearts King, Card Leaves Nine])
        (Card Hearts King)
        `shouldBe` Unplayable (FollowTrump Bells)
    it "returns 'FollowLead' if card doesn't follow the lead" $ do
      Card.status
        (Trump Bells)
        [Card Leaves Over, Card Acorns Seven, Card Bells Over]
        (Set.fromList [Card Bells Ten, Card Hearts King, Card Leaves Nine])
        (Card Hearts King)
        `shouldBe` Unplayable (FollowLead Leaves)
    it "returns 'Undertrump' if card would undertrump" $
      Card.status
        (Trump Bells)
        [Card Leaves Over, Card Acorns Seven, Card Bells Over]
        (Set.fromList [Card Bells Ten, Card Bells King, Card Hearts King, Card Leaves Nine])
        (Card Bells Ten)
        `shouldBe` Unplayable (Undertrump (Card Bells Over))
    it "returns 'Playable' if unable to follow trump" $
      Card.status
        (Trump Bells)
        [Card Bells Over, Card Acorns Seven]
        (Set.fromList [Card Acorns Ten, Card Leaves King, Card Hearts King])
        (Card Acorns Ten)
        `shouldBe` Playable
    it "returns 'Playable' if unable to follow lead" $
      Card.status
        (Trump Bells)
        [Card Hearts Over, Card Acorns Seven, Card Bells Over]
        (Set.fromList [Card Acorns Ten, Card Leaves King, Card Bells Eight])
        (Card Acorns Ten)
        `shouldBe` Playable
    it "returns 'Playable' if forced to undertrump" $
      Card.status
        (Trump Bells)
        [Card Hearts Over, Card Acorns Seven, Card Bells Over]
        (Set.fromList [Card Bells Ten, Card Bells Six])
        (Card Bells Ten)
        `shouldBe` Playable

  describe "isPlayable" $ do
    prop "matches 'Card.status'" $ \v t h c ->
      Card.isPlayable v t h c === (Card.status v t h c == Playable)
-}

allVariants :: [Variant]
allVariants =
  (Trump <$> [minBound .. maxBound])
    <> ([Direction, Slalom] <*> [TopDown, BottomUp])

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
