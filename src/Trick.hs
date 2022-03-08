module Trick
  ( Trick,
    play,
    winner,
    points,
  )
where

import Action
import Card (Card, Cards)
import Card qualified
import Control.Monad.Except
import Control.Monad.State (MonadState, get, modify)
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Data.Vector.Sized qualified as Vector
import GHC.TypeLits (KnownNat)
import Jass
import JassNat (JassNat)
import Lens (over)
import List qualified
import Set qualified
import Variant (Variant)

data Trick n = Trick
  { variant :: Variant,
    leader :: Finite n,
    cards :: Vector n Card
  }
  deriving (Eq, Show)

play :: (MonadJass n m, MonadState (Vector n Cards) m) => Variant -> Finite n -> m (Trick n)
play variant leader = playCard []
  where
    playCard cards = maybe (go cards) (pure . close) $ Vector.fromList cards
    close cards = Trick {variant, leader, cards = rotate (negate leader) cards}
    go cards = do
      hands <- get
      let player = leader + fromIntegral (length cards)
          hand = Vector.index hands player
          gameView ix =
            GameView
              { trick =
                  rotate ix $
                    Vector.accum
                      (const Just)
                      (Vector.replicate Nothing)
                      (List.zip [leader ..] cards),
                variant = Just variant,
                hand = Vector.index hands ix
              }
      card <- prompt player gameView $ \case
        ChooseVariant _ -> throwError VariantAlreadyDefined
        PlayCard c -> do
          case Card.status variant cards hand c of
            Card.Unplayable reason -> throwError $ CardUnplayable reason
            Card.Playable -> pure c
      modify (over (Vector.ix player) (Set.delete card))
      playCard (List.snoc cards card)

winner :: JassNat n => Trick n -> Finite n
winner trick = Vector.maxIndexBy (Card.compare trick.variant lead) trick.cards
  where
    lead = Card.suit $ Vector.index trick.cards trick.leader

points :: Trick n -> Int
points trick = Vector.sum $ Vector.map (Card.value trick.variant) trick.cards

rotate :: KnownNat n => Finite n -> Vector n a -> Vector n a
rotate n v = Vector.generate (\i -> Vector.index v $ i + n)
