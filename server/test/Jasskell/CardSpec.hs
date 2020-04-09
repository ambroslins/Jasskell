{-# LANGUAGE DataKinds #-}

module Jasskell.CardSpec
    ( spec
    )
where

import           Data.Aeson                     ( encode
                                                , decode
                                                )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.Set                      as Set
import           Test.Hspec
import           Test.QuickCheck
import           Jasskell.Card.Internal
import           Jasskell.Card.Suit
import           Jasskell.Card.Rank
import           Jasskell.Variant

instance Arbitrary Suit where
    arbitrary = elements [Bells, Hearts, Acorns, Leaves]

instance Arbitrary Rank where
    arbitrary = elements [Six .. Ace]

instance Arbitrary Variant where
    arbitrary = elements
        [ Trump Bells
        , Trump Hearts
        , Trump Acorns
        , Trump Leaves
        , Direction TopDown
        , Direction BottomUp
        , Slalom TopDown
        , Slalom BottomUp
        ]

instance Arbitrary Card where
    arbitrary = Card <$> arbitrary <*> arbitrary

spec :: Spec
spec = describe "Card" $ do
    describe "allCards" $ it "contains 36 cards" (length allCards `shouldBe` 36)
    describe "value" $ it "of all cards is 152" $ mapM_
        (\var -> sum (map (value var) allCards) `shouldBe` 152)
        [ Trump Bells
        , Trump Hearts
        , Trump Acorns
        , Trump Leaves
        , Direction TopDown
        , Direction BottomUp
        , Slalom TopDown
        , Slalom BottomUp
        ]
    describe "compareCard" $ do
        it "is transitive " $ property $ \v l c1 c2 c3 ->
            let comp = compareCard v l
            in  comp c1 c2 == comp c2 c3 ==> comp c1 c3 == comp c1 c2
        it "is reflexive" $ property $ \v l c -> compareCard v l c c == EQ
        it "is not commutative" $ property $ \v l c1 c2 ->
            let comp = compareCard v l
                inv EQ = EQ
                inv LT = GT
                inv GT = LT
            in  comp c1 c2 == inv (comp c2 c1)

        context "when the variant is trump" $ do
            it "trump card greater then non trump card"
                $ property
                $ \c1 c2 l ->
                      suit c1
                          /=  suit c2
                          ==> compareCard (Trump $ suit c1) l c1 c2
                          ==  GT
            it "non trump cards compare like TopDown"
                $ property
                $ \t l c1 c2 ->
                      suit c1
                          /=  t
                          &&  suit c2
                          /=  t
                          ==> compareCard (Trump t)           l c1 c2
                          ==  compareCard (Direction TopDown) l c1 c2
        context "when the variant is TopDown" $ do
            let comp = compareCard (Direction TopDown)
            it "lead card > non lead card" $ property $ \c1 c2 ->
                suit c1 /= suit c2 ==> comp (suit c1) c1 c2 == GT
            it "lead cards compare on rank" $ property $ \s r1 r2 ->
                let c1 = Card s r1
                    c2 = Card s r2
                in  comp s c1 c2 == compare r1 r2
        context "when the variant is BottomUp" $ do
            let comp = compareCard (Direction BottomUp)
            it "lead cards compare on rank flipped" $ property $ \s r1 r2 ->
                let c1 = Card s r1
                    c2 = Card s r2
                in  comp s c1 c2 == compare r2 r1
    describe "highestCard" $ do
        it "is one of the given cards" $ property $ \v l c cs ->
            highestCard v l (c :| cs) `elem` (c : cs)
        it "is geater or equal to all other cards" $ property $ \v l c cs -> all
            (\x -> compareCard v l (highestCard v l (c :| cs)) x /= LT)
            (c : cs)
    describe "playableCards" $ do
        it "is not empty if hand is not empty" $ property $ \v l cs hand ->
            not (Set.null hand) ==> not (Set.null $ playableCards v l cs hand)
        it "is the hand if table is empty" $ property $ \v l hand ->
            playableCards v l [] hand == hand
    it "json decode is invers to encode" $ property $ \c ->
        (decode . encode) c `shouldBe` (Just c :: Maybe Card)
