module Jasskell.Game
  ( Game,
    rounds,
    Interface (..),
    play,
    rotate,
  )
where

import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Jasskell.Card (BadCard, Card, Cards)
import Jasskell.Declaration (BadDeclaration, Declaration)
import Jasskell.Declaration qualified as Declaration
import Jasskell.Jass (JassNat)
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
import Jasskell.Trick (Hands)
import Jasskell.Variant (Variant)
import Jasskell.View.Declaring (ViewDeclaring)
import Jasskell.View.Declaring qualified as View.Declaring
import Jasskell.View.Playing (ViewPlaying)
import Jasskell.View.Playing qualified as View.Playing
import Jasskell.Views (Views)

newtype Game n = Game {rounds :: [Round n]}
  deriving (Show)

data Interface n m = Interface
  { promptCard :: Finite n -> Views ViewPlaying n -> m Card,
    promptDeclaration :: Finite n -> Views ViewDeclaring n -> m Declaration,
    deal :: m (Vector n Cards),
    throwBadCard :: forall a. BadCard -> m a,
    throwBadDeclaration :: forall a. BadDeclaration -> m a
  }

play :: forall n m. (JassNat n, Monad m) => Interface n m -> m (Game n)
play Interface {..} = go [] 0
  where
    go rs leader =
      -- TODO: check if one team has reached the point limit
      if length rs >= 10
        then pure Game {rounds = rs}
        else do
          hands <- deal
          variant <- declareVariant rs leader hands
          r <- playRound rs variant leader hands
          go (r : rs) (leader + 1)

    declareVariant :: [Round n] -> Finite n -> Hands n -> m Variant
    declareVariant rounds eldest hands =
      Declaration.declareVariant interface (eldest :| [])
      where
        makeView = View.Declaring.makeViews hands rounds eldest
        interface =
          Declaration.Interface
            { Declaration.promptDeclaration = \current ns ->
                promptDeclaration current (makeView (current :| ns)),
              Declaration.throwBadDeclaration = throwBadDeclaration
            }

    playRound :: [Round n] -> Variant -> Finite n -> Hands n -> m (Round n)
    playRound rounds variant leader hands =
      Round.play interface variant leader hands
      where
        interface =
          Round.Interface
            { Round.promptCard = \current views ->
                promptCard current (View.Playing.makeViews rounds views),
              Round.throwBadCard = throwBadCard
            }

rotate :: KnownNat n => Finite n -> Game n -> Game n
rotate i = coerce (map (Round.rotate i))
