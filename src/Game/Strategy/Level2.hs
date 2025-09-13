-- | Level 2: Simple elimination strategy
module Game.Strategy.Level2
  ( chooseEliminationGuess
  ) where

import Game.Types
import Game.State

-- | Simple elimination - pick first unguessed candidate
chooseEliminationGuess :: [Number] -> [GuessHistory] -> Number
chooseEliminationGuess candidates history =
  let unguessedCandidates = filter (`notInHistory` history) candidates
  in case unguessedCandidates of
       [] -> head candidates
       (x:_) -> x
  where
    notInHistory num hist = not $ any (\(GuessHistory pastGuess _) -> pastGuess == num) hist