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
        then pure $ Game rs
        else do
          hands <- deal
          variant <- declareVariant rs hands leader
          r <- playRound rs hands leader variant
          go (r : rs) (leader + 1)

    declareVariant :: [Round n] -> Vector n Cards -> Finite n -> m Variant
    declareVariant rounds hands eldest =
      Declaration.declareVariant interface (eldest :| [])
      where
        makeView = View.Declaring.make hands rounds eldest
        interface =
          Declaration.Interface
            { Declaration.promptDeclaration = \current ps ->
                promptDeclaration current $ makeView (current :| ps),
              Declaration.throwBadDeclaration = throwBadDeclaration
            }

    playRound :: [Round n] -> Vector n Cards -> Finite n -> Variant -> m (Round n)
    playRound rounds hands leader variant =
      Round.play interface variant leader hands
      where
        interface =
          Round.Interface
            { Round.promptCard = \current view ->
                promptCard current $
                  View.Playing.make rounds view,
              Round.throwBadCard = throwBadCard
            }

rotate :: KnownNat n => Finite n -> Game n -> Game n
rotate i = coerce $ map (Round.rotate i)
