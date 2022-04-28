module View.Playing
  ( Playing,
    hand,
    variant,
    leader,
    table,
    current,
    make,
    PlayableCard,
    unvalidateCard,
    Reason,
    validateCard,
    playableCards,
  )
where

import Card (Card, Cards, Suit)
import Card qualified
import Data.Finite (Finite)
import Data.Foldable (maximumBy)
import Data.Set qualified as Set
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import Variant (Variant (..))

data Playing n = Playing
  { hand :: Cards,
    variant :: Variant,
    leader :: Finite n,
    table :: [Card]
  }
  deriving (Show)

make :: KnownNat n => Vector n Cards -> Finite n -> Variant -> [Card] -> Finite n -> Playing n
make hands absoluteLeader variant table i =
  Playing
    { hand = Vector.index hands i,
      variant,
      leader = absoluteLeader - i,
      table
    }

current :: KnownNat n => Playing n -> Finite n
current Playing {leader, table} = leader + fromIntegral (length table)

newtype PlayableCard = PlayableCard {unvalidateCard :: Card}
  deriving (Eq, Ord, Show)

data Reason
  = NotInHand
  | NotYourTurn
  | FollowTrump Suit
  | FollowLead Suit
  | Undertrump Card
  deriving (Eq, Show)

validateCard :: KnownNat n => Playing n -> Card -> Either Reason PlayableCard
validateCard view@Playing {hand, variant, table} card
  | current view /= 0 = Left NotYourTurn
  | Set.notMember card hand = Left NotInHand
  | otherwise = case table of
    [] -> pure $ PlayableCard card
    (c : cs) -> case variant of
      Trump trump
        | lead == trump ->
          check (FollowTrump trump) $
            Card.suit card == trump || null (Set.delete puur trumps)
        | Card.suit highest == trump ->
          if Card.suit card == trump
            then
              check (Undertrump highest) $
                comp card highest == GT
                  || (null highers && null (Set.difference hand trumps))
            else
              check (FollowLead lead) $
                Card.suit card == lead || null followers
        | otherwise ->
          check (FollowLead lead) $
            Card.suit card `elem` [lead, trump] || null followers
        where
          trumps = Set.filter ((== trump) . Card.suit) hand
          highest = maximumBy comp (c :| cs)
          highers = Set.filter (\x -> comp x highest == GT) hand
          puur = Card.Card trump Card.Under
      _ ->
        check (FollowLead lead) $
          Card.suit card == lead || null followers
      where
        lead = Card.suit c
        followers = Set.filter ((== lead) . Card.suit) hand
        comp = Card.compare variant lead
        check r p = if p then pure (PlayableCard card) else Left r

playableCards :: KnownNat n => Playing n -> Set PlayableCard
playableCards view = Set.fromList . rights . map (validateCard view) . Set.toList $ View.Playing.hand view