{-# LANGUAGE DeriveGeneric #-}

module Game.Model
  ( AppState(..)
  , initialState
  , resetGameState
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import Game.Types
import Game.Config
import Game.State
import Game.Number (allPossibleNumbers)
import Game.Input (InputState(..))

-- | Main application state
data AppState = AppState
  { config        :: !GameConfig       -- Game configuration (size, level)
  , playerSecret  :: !(Maybe Number)   -- Player's secret number
  , cpuSecret     :: !(Maybe Number)   -- CPU's secret number
  , currentInput  :: !InputState       -- Current input state
  , playerHistory :: ![GuessHistory]   -- Player's guess history
  , cpuHistory    :: ![GuessHistory]   -- CPU's guess history
  , cpuState      :: !GameState        -- CPU's internal state
  , currentTurn   :: !Player           -- Current player's turn
  , outcome       :: !GameOutcome      -- Game outcome
  , message       :: !Text             -- UI message
  , showConfig    :: !Bool             -- Show configuration panel
  , candidateHistory :: ![Int]         -- History of candidate counts for graph
  } deriving (Show, Eq, Generic)

-- | Initial application state
initialState :: AppState
initialState = AppState
  { config = defaultConfig
  , playerSecret = Nothing
  , cpuSecret = Nothing
  , currentInput = Empty
  , playerHistory = []
  , cpuHistory = []
  , cpuState = initGameState (numberSize defaultConfig)
  , currentTurn = Human
  , outcome = Ongoing
  , message = "Choose game settings or start with defaults"
  , showConfig = True
  , candidateHistory = [length $ allPossibleNumbers (numberSize defaultConfig)]
  }

-- | Reset game state with current configuration
resetGameState :: AppState -> AppState
resetGameState s = initialState
  { config = config s
  , cpuState = initGameState (numberSize $ config s)
  , message = "New game! Enter YOUR secret number first"
  , showConfig = False
  , candidateHistory = [length $ allPossibleNumbers (numberSize $ config s)]
  }