-- | Level 1: Random guessing strategy
module Game.Strategy.Level1
  ( chooseRandomGuess
  ) where

import System.Random
import Game.Types
import Game.State

-- | Random guess selection (excluding past guesses)
chooseRandomGuess :: [Number] -> [GuessHistory] -> IO Number
chooseRandomGuess candidates history = do
  let unguessedCandidates = filter (`notInHistory` history) candidates
      candidatesToChoose = if null unguessedCandidates then candidates else unguessedCandidates
  index <- randomRIO (0, length candidatesToChoose - 1)
  return $ candidatesToChoose !! index
  where
    notInHistory num hist = not $ any (\(GuessHistory pastGuess _) -> pastGuess == num) hist