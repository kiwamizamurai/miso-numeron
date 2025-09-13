{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Update
  ( updateModel
  ) where

import Miso
import Control.Monad (when)
import Control.Monad.State (runState, execState)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T

import Game.Types
import Game.Config
import Game.Number
import Game.State
import Game.Model
import Game.Actions
import Game.CPU
import Game.Input
import System.Random (randomRIO)

-- | Main update function
updateModel :: Action -> Transition AppState Action
updateModel NoOp = pure ()

updateModel (InputChanged input) = do
  -- Legacy text input handler, now mostly unused
  pure ()

updateModel (NumPadDigitClicked digit) = do
  s <- get
  let newInput = addDigit (numberSize $ config s) digit (currentInput s)
  put $ s { currentInput = newInput }

updateModel NumPadClear = do
  s <- get
  put $ s { currentInput = clearInput (currentInput s) }

updateModel NumPadBackspace = do
  s <- get
  put $ s { currentInput = removeLastDigit (currentInput s) }

updateModel SubmitGuess = do
  s <- get
  when (outcome s == Ongoing) $ handleGuess s

updateModel SetPlayerSecret = do
  s <- get
  let numSize = numberSize (config s)
  case getNumber (currentInput s) of
    Nothing -> put $ s { message = "Invalid! Use " <> T.pack (show $ numberSizeToInt numSize) <> " different digits" }
    Just number -> put $ s 
      { playerSecret = Just number
      , currentInput = Empty
      , message = "Your turn! Guess CPU's number"
      }

updateModel CPUTurn = do
  s <- get
  when (outcome s == Ongoing) $ handleCPUTurn s

updateModel (SetCPUSecret code) = do
  s <- get
  put $ s { cpuSecret = Just code }

updateModel ResetGame = do
  s <- get
  let newState = resetGameState s
#if !defined(__GHCJS__) && !defined(wasm32_HOST_ARCH)
  -- For native builds, generate random CPU secret
  scheduleIO $ SetCPUSecret <$> generateRandomNumber (numberSize $ config newState)
#else
  -- For WASM/GHCJS, use a pseudo-random code
  let numSize = numberSize (config newState)
      pseudoSecret = case numSize of
        Three -> fromJust $ mkNumber [9,8,7]
        Four -> fromJust $ mkNumber [9,8,7,6]
        Five -> fromJust $ mkNumber [9,8,7,6,5]
  put $ newState { cpuSecret = Just pseudoSecret }
#endif

updateModel ToggleConfig = do
  s <- get
  put $ s { showConfig = not (showConfig s) }

updateModel (SetNumberSize size) = do
  s <- get
  let newConfig = (config s) { numberSize = size }
  put $ s 
    { config = newConfig
    , cpuState = initGameState size
    , candidateHistory = [length $ allPossibleNumbers size]
    }

updateModel (SetGameLevel level) = do
  s <- get
  let newConfig = (config s) { gameLevel = level }
  put $ s { config = newConfig }

updateModel StartGameWithConfig = do
  s <- get
  let newState = s 
        { showConfig = False
        , message = "Enter YOUR secret number first"
        , cpuState = initGameState (numberSize $ config s)
        }
#if !defined(__GHCJS__) && !defined(wasm32_HOST_ARCH)
  scheduleIO $ SetCPUSecret <$> generateRandomNumber (numberSize $ config newState)
#else
  let numSize = numberSize (config newState)
      pseudoSecret = case numSize of
        Three -> fromJust $ mkNumber [9,8,7]
        Four -> fromJust $ mkNumber [9,8,7,6]
        Five -> fromJust $ mkNumber [9,8,7,6,5]
  put $ newState { cpuSecret = Just pseudoSecret }
#endif

-- | Handle player guess
handleGuess :: AppState -> Transition AppState Action
handleGuess s = do
  case playerSecret s of
    Nothing -> updateModel SetPlayerSecret
    Just _ -> do
      let numSize = numberSize (config s)
      case (getNumber (currentInput s), cpuSecret s) of
        (Just guess, Just secret) -> do
          let hint = calculateHint secret guess
              history = GuessHistory guess hint
              newState = s 
                { playerHistory = playerHistory s ++ [history]
                , currentInput = Empty
                }
          
          if isWinning numSize hint
            then put $ newState 
              { outcome = Victory Human
              , message = "You win! You found " <> T.pack (formatNumber secret) <> "!"
              }
            else do
              put $ newState 
                { currentTurn = CPU
                , message = T.pack (show $ eat hint) <> " EAT, " <> 
                           T.pack (show $ bite hint) <> " BITE. CPU is thinking..."
                }
              -- Execute CPU turn immediately
              s' <- get
              handleCPUTurn s'
        _ -> put $ s { message = "Invalid input!" }

-- | Handle CPU turn
handleCPUTurn :: AppState -> Transition AppState Action
handleCPUTurn s = do
  case playerSecret s of
    Just playerSec -> do
      -- For simplicity in WASM, use deterministic CPU (first candidate)
      let candidates = possibilities (cpuState s)
      case candidates of
        [] -> put $ s { message = "Error: No possible numbers!" }
        (guess:_) -> do
          let hint = calculateHint playerSec guess
              history = GuessHistory guess hint
              numSize = numberSize (config s)
              -- Update CPU state with the guess result
              updatedCpuState = execState (updatePossibilities guess hint) (cpuState s)
              
          if isWinning numSize hint
            then put $ s 
              { cpuHistory = cpuHistory s ++ [history]
              , cpuState = updatedCpuState
              , outcome = Victory CPU
              , message = "CPU wins! The number was " <> T.pack (formatNumber guess)
              }
            else put $ s 
              { cpuHistory = cpuHistory s ++ [history]
              , cpuState = updatedCpuState
              , currentTurn = Human
              , candidateHistory = candidateHistory s ++ [length $ possibilities updatedCpuState]
              , message = "CPU guessed " <> T.pack (formatNumber guess) <> ": " <>
                         T.pack (show $ eat hint) <> " EAT, " <>
                         T.pack (show $ bite hint) <> " BITE. " <>
                         T.pack (show $ length $ possibilities updatedCpuState) <> 
                         " candidates left. Your turn!"
              }
    _ -> pure ()

-- | Generate random number for given size
generateRandomNumber :: NumberSize -> IO Number
generateRandomNumber numSize = do
  let digitCount = numberSizeToInt numSize
      availableDigits = [0..9]
  firstDigit <- randomRIO (1, 9)
  let remainingDigits = filter (/= firstDigit) availableDigits
  randomDigits <- selectRandomN (digitCount - 1) remainingDigits
  return $ fromJust $ mkNumber (firstDigit : randomDigits)
  where
    selectRandomN :: Int -> [Int] -> IO [Int]
    selectRandomN 0 _ = return []
    selectRandomN n xs = do
      idx <- randomRIO (0, length xs - 1)
      let (before, selected:after) = splitAt idx xs
      rest <- selectRandomN (n - 1) (before ++ after)
      return (selected : rest)