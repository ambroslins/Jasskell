module Jasskell.Game where

import Control.Monad.Except (MonadError (throwError))
import Data.Finite (Finite)
import Data.Vector.Sized (Vector)
import Jasskell.Card (BadCard, Card, Cards)
import Jasskell.Declaration (BadDeclaration, Declaration)
import Jasskell.Declaration qualified as Declaration
import Jasskell.Jass (JassNat, deal)
import Jasskell.Round (Round)
import Jasskell.Round qualified as Round
import Jasskell.Variant (Variant)
import Jasskell.View.Declaring (Declaring)
import Jasskell.View.Declaring qualified as View.Declaring
import Jasskell.View.Playing (Playing)
import Jasskell.View.Playing qualified as View.Playing
import Jasskell.Views (Views)
import System.Random (RandomGen)

newtype Game n = Game {rounds :: [Round n]}
  deriving (Show)

data Error
  = BadCard BadCard
  | BadDeclaration BadDeclaration
  deriving (Eq, Show)

data Interface n m = Interface
  { promptCard :: Finite n -> Views Playing n -> m Card,
    promptDeclaration :: Finite n -> Views Declaring n -> m Declaration
  }

play ::
  forall n m g.
  (JassNat n, MonadError Error m, RandomGen g) =>
  Interface n m ->
  g ->
  m (Game n)
play Interface {..} = evalStateT $ go [] 0
  where
    go rs leader =
      -- TODO: check if one team has reached the point limit
      if length rs >= 10
        then pure $ Game rs
        else do
          hands <- state deal
          variant <- lift $ declareVariant rs hands leader
          r <- lift $ playRound rs hands leader variant
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
              Declaration.throwBadDeclaration = throwError . BadDeclaration
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
              Round.throwBadCard = throwError . BadCard
            }
