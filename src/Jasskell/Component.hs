module Jasskell.Component (card) where

import Jasskell.Card (Card, Rank (..), Suit (..))
import Jasskell.Card qualified as Card
import Jasskell.Icon qualified as Icon
import Lucid

card :: (Monad m) => [Attributes] -> Card -> HtmlT m ()
card as c = div_ (class_ "card" : as) $ do
  span_ [class_ "rank top"] $ rank <> suit
  span_ [class_ "suit"] suit
  span_ [class_ "rank bottom"] $ rank <> suit
  where
    suit = case Card.suit c of
      Bells -> Icon.bell []
      Acorns -> Icon.acorn []
      Leaves -> Icon.leaf []
      Hearts -> Icon.heart []
    rank = case Card.rank c of
      Six -> "6"
      Seven -> "7"
      Eight -> "8"
      Nine -> "9"
      Ten -> "10"
      Under -> "U"
      Over -> "O"
      King -> "K"
      Ace -> "A"
