module Jasskell.Trick
  ( Trick,
    cards,
    leader,
    winner,
    points,
    close,
  )
where

import Data.Vector4 (Index4, Vector4)
import Data.Vector4 qualified as Vector4
import Jasskell.Card (Card)
import Jasskell.Card qualified as Card
import Jasskell.Variant (Variant)

data Trick = Trick
  { cards :: !(Vector4 Card),
    leader :: !Index4,
    winner :: !Index4,
    points :: !Int
  }
  deriving (Show)

close :: Variant -> Index4 -> Vector4 Card -> Trick
close v leader cards =
  Trick
    { leader,
      cards,
      winner =
        leader
          + Vector4.maxIndexBy
            (Card.compare lead v)
            (Vector4.rotate leader cards),
      points = foldl' (\p c -> p + Card.points v c) 0 cards
    }
  where
    lead = Card.suit $ Vector4.index leader cards
