-- | CPU AI logic for Numeron game
module Game.CPU
  ( cpuMakeGuess
  ) where

import Game.Types
import Game.Config
import Game.State
import Game.Strategy.Level1
import Game.Strategy.Level2
import Game.Strategy.Level3

-- | CPU makes a strategic guess based on current game state and level
cpuMakeGuess :: GameLevel -> GameState -> IO CpuGuessResult
cpuMakeGuess level gameState = do
  case possibilities gameState of
    [] -> return NoPossibilities
    [single] -> return $ CpuGuessResult single gameState
    candidates -> do
      bestGuess <- case level of
        Level1 -> chooseRandomGuess candidates (previousGuesses gameState)
        Level2 -> return $ chooseEliminationGuess candidates (previousGuesses gameState)
        Level3 -> return $ chooseOptimalGuess candidates (previousGuesses gameState)
      return $ CpuGuessResult bestGuess gameState