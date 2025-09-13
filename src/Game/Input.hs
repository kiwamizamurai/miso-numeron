{-# LANGUAGE PatternSynonyms #-}

module Game.Input
  ( Digit(..)
  , mkDigit
  , digitToInt
  , digitToText
  , allDigits
  , pattern D0, pattern D1, pattern D2, pattern D3, pattern D4
  , pattern D5, pattern D6, pattern D7, pattern D8, pattern D9
  , InputState(..)
  , addDigit
  , removeLastDigit
  , clearInput
  , inputToText
  , isDigitUsed
  , canAddMore
  , getNumber
  , fromNumber
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Game.Types (Number, mkNumber)
import Game.Config (NumberSize, numberSizeToInt)
import Game.Number (formatNumber)

-- | A digit from 0 to 9 with compile-time guarantees
newtype Digit = Digit Int
  deriving (Eq, Ord, Show)

-- | Smart constructor for Digit
mkDigit :: Int -> Maybe Digit
mkDigit n
  | n >= 0 && n <= 9 = Just (Digit n)
  | otherwise = Nothing

-- | Pattern synonyms for convenient digit matching
pattern D0, D1, D2, D3, D4, D5, D6, D7, D8, D9 :: Digit
pattern D0 = Digit 0
pattern D1 = Digit 1
pattern D2 = Digit 2
pattern D3 = Digit 3
pattern D4 = Digit 4
pattern D5 = Digit 5
pattern D6 = Digit 6
pattern D7 = Digit 7
pattern D8 = Digit 8
pattern D9 = Digit 9

-- | Extract Int from Digit
digitToInt :: Digit -> Int
digitToInt (Digit n) = n

-- | Convert Digit to Text
digitToText :: Digit -> Text
digitToText (Digit n) = T.pack (show n)

-- | All possible digits
allDigits :: [Digit]
allDigits = [D0, D1, D2, D3, D4, D5, D6, D7, D8, D9]

-- | Input state with type-level guarantees
data InputState
  = Empty                           -- No input yet
  | Partial [Digit]                 -- Partial input (list of unique digits)
  | Complete Number                 -- Complete valid number
  deriving (Eq, Show)

-- | Add a digit to the input state
addDigit :: NumberSize -> Digit -> InputState -> InputState
addDigit _ _ (Complete n) = Complete n  -- Already complete
addDigit _ d Empty = Partial [d]
addDigit maxSize d (Partial digits)
  | d `elem` digits = Partial digits  -- Duplicate, ignore
  | length digits + 1 >= numberSizeToInt maxSize = 
      case mkNumber (map digitToInt (digits ++ [d])) of
        Just n -> Complete n
        Nothing -> Partial digits  -- Should not happen with valid digits
  | otherwise = Partial (digits ++ [d])

-- | Remove the last digit
removeLastDigit :: InputState -> InputState
removeLastDigit Empty = Empty
removeLastDigit (Complete _) = Empty  -- Reset if complete
removeLastDigit (Partial []) = Empty
removeLastDigit (Partial digits) = 
  case init digits of
    [] -> Empty
    ds -> Partial ds

-- | Clear all input
clearInput :: InputState -> InputState
clearInput _ = Empty

-- | Convert input state to display text
inputToText :: InputState -> Text
inputToText Empty = ""
inputToText (Partial digits) = T.concat $ map digitToText digits
inputToText (Complete n) = T.pack $ formatNumber n

-- | Check if a digit is already used
isDigitUsed :: Digit -> InputState -> Bool
isDigitUsed _ Empty = False
isDigitUsed d (Partial digits) = d `elem` digits
isDigitUsed _ (Complete _) = True  -- All digits are "used" when complete

-- | Get Number from InputState if complete
getNumber :: InputState -> Maybe Number
getNumber (Complete n) = Just n
getNumber _ = Nothing

-- | Create InputState from Number
fromNumber :: Number -> InputState
fromNumber = Complete

-- | Check if more digits can be added
canAddMore :: NumberSize -> InputState -> Bool
canAddMore _ Empty = True
canAddMore _ (Complete _) = False
canAddMore maxSize (Partial digits) = length digits < numberSizeToInt maxSize