module Game.Actions where

import Data.Text (Text)
import Game.Types hiding (Digit)
import Game.Config
import Game.Input (Digit)

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
  | NumPadDigitClicked Digit        -- Number pad digit clicked (type-safe 0-9)
  | NumPadClear                     -- Clear all input
  | NumPadBackspace                 -- Remove last digit
  deriving (Eq, Show)