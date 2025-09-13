{-# LANGUAGE DeriveGeneric #-}

module Game.Types
  ( Number(..)
  , Digit(..)
  , Hint(..)
  , GuessHistory(..)
  , GameOutcome(..)
  , Player(..)
  , mkNumber
  , numberToList
  , digitToInt
  , intToDigit
  ) where

import GHC.Generics (Generic)
import Data.List (nub)

-- | A single digit (0-9)
newtype Digit = Digit Int deriving (Eq, Ord, Generic)

instance Show Digit where
  show (Digit n) = show n

-- | A number with unique digits
newtype Number = Number [Digit] deriving (Eq, Generic)

instance Show Number where
  show (Number digits) = concatMap show digits

-- | Hint returned after a guess (EAT and BITE counts)
data Hint = Hint
  { eat :: Int   -- Correct digit in correct position
  , bite :: Int  -- Correct digit in wrong position
  } deriving (Show, Eq, Ord, Generic)

-- | History of a guess and its result
data GuessHistory = GuessHistory
  { guess :: Number
  , hint :: Hint
  } deriving (Show, Eq, Generic)

-- | Game outcome
data GameOutcome
  = Ongoing
  | Victory Player
  deriving (Show, Eq, Generic)

-- | Player type
data Player = Human | CPU
  deriving (Show, Eq, Generic)

-- | Create a Number from a list of integers
mkNumber :: [Int] -> Maybe Number
mkNumber xs
  | all isValidDigit xs && hasUniqueDigits xs = 
      Just $ Number (map Digit xs)
  | otherwise = Nothing
  where
    isValidDigit n = n >= 0 && n <= 9
    hasUniqueDigits ys = length ys == length (nub ys)

-- | Convert Number to list of integers
numberToList :: Number -> [Int]
numberToList (Number digits) = map digitToInt digits

-- | Convert Digit to Int
digitToInt :: Digit -> Int
digitToInt (Digit n) = n

-- | Convert Int to Digit (unsafe, assumes valid range)
intToDigit :: Int -> Digit
intToDigit = Digit