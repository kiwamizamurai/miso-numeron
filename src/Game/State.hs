{-# LANGUAGE DeriveGeneric #-}

module Game.State
  ( GameState(..)
  , CpuGuessResult(..)
  , initGameState
  , updatePossibilities
  , isConsistentWithHistory
  ) where

import GHC.Generics (Generic)
import Control.Monad.State
import Game.Types
import Game.Number
import Game.Config

-- | Game state tracking possible numbers and previous guesses  
data GameState = GameState
  { possibilities :: [Number]
  , previousGuesses :: [GuessHistory]
  } deriving (Show, Eq, Generic)

-- | Result of CPU making a guess
data CpuGuessResult
  = CpuGuessResult
      { cpuGuess :: Number
      , newGameState :: GameState
      }
  | NoPossibilities
  deriving (Show, Eq, Generic)

-- | Initialize game state with all possibilities for specified size
initGameState :: NumberSize -> GameState
initGameState numSize = GameState (allPossibleNumbers numSize) []

-- | Update game possibilities based on the last guess and hint
updatePossibilities :: Number -> Hint -> State GameState ()
updatePossibilities lastGuess lastHint = do
  gameState <- get
  let newHistory = GuessHistory lastGuess lastHint : previousGuesses gameState
      newPossibilities = filter (`isConsistentWithHistory` newHistory) (possibilities gameState)
  put $ GameState newPossibilities newHistory

-- | Check if a candidate number is consistent with guess history
isConsistentWithHistory :: Number -> [GuessHistory] -> Bool
isConsistentWithHistory candidate =
  all (\(GuessHistory pastGuess expectedHint) -> calculateHint candidate pastGuess == expectedHint)