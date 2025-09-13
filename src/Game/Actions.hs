module Game.Actions where

import Data.Text (Text)
import Game.Types
import Game.Config

-- | User actions
data Action
  = NoOp
  | InputChanged Text
  | SubmitGuess
  | SetPlayerSecret
  | CPUTurn
  | SetCPUSecret Number
  | ResetGame
  | ToggleConfig                    -- Show/hide configuration panel
  | SetNumberSize NumberSize        -- Change number size
  | SetGameLevel GameLevel          -- Change difficulty level
  | StartGameWithConfig             -- Start game with selected configuration
  deriving (Eq, Show)