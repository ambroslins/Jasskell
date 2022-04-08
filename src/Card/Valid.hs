module Card.Valid
  ( ValidCard,
    unvalidate,
    validate,
    valids,
  )
where

import Card (Card (..), Rank (..), Suit)
import Card qualified
import Data.Foldable (maximumBy)
import Data.Set qualified as Set
import Variant (Variant (Trump))
import View (Phase (..), View)
import View qualified

newtype ValidCard = ValidCard {unvalidate :: Card}
  deriving (Eq, Ord, Show)

data Reason
  = NotInHand
  | NotYourTurn
  | FollowTrump Suit
  | FollowLead Suit
  | Undertrump Card
  deriving (Eq, Show)

validate :: KnownNat n => View 'Playing n -> Card -> Either Reason ValidCard
validate view card
  | View.current view /= 0 = Left NotYourTurn
  | Set.notMember card (View.hand view) = Left NotInHand
  | otherwise = case View.table view of
    [] -> pure $ ValidCard card
    (c : cs) -> case View.variant view of
      Trump trump
        | lead == trump ->
          check (FollowTrump trump) $
            suit card == trump || null (Set.delete puur trumps)
        | suit highest == trump ->
          if suit card == trump
            then
              check (Undertrump highest) $
                comp card highest == GT
                  || (null highers && null (Set.difference hand trumps))
            else
              check (FollowLead lead) $
                suit card == lead || null followers
        | otherwise ->
          check (FollowLead lead) $
            suit card `elem` [lead, trump] || null followers
        where
          trumps = Set.filter ((== trump) . suit) hand
          highest = maximumBy comp (c :| cs)
          highers = Set.filter (\x -> comp x highest == GT) hand
          puur = Card trump Under
      _ ->
        check (FollowLead lead) $
          suit card == lead || null followers
      where
        hand = View.hand view
        lead = suit c
        followers = Set.filter ((== lead) . suit) hand
        comp = Card.compare (View.variant view) lead
        check r p = if p then pure (ValidCard card) else Left r

valids :: KnownNat n => View 'Playing n -> Set ValidCard
valids view = Set.fromList . rights . map (validate view) . Set.toList $ View.hand view