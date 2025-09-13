module Game.Number
  ( calculateHint
  , allPossibleNumbers
  , isValidNumber
  , formatNumber
  , parseNumber
  , isWinning
  ) where

import Game.Types
import Game.Config
import Data.List (nub)
import Data.Maybe (isJust)

-- | Calculate EAT and BITE hints for a guess
calculateHint :: Number -> Number -> Hint
calculateHint target guess = Hint eatCount biteCount
  where
    targetList = numberToList target
    guessList = numberToList guess
    eatCount = length $ filter (uncurry (==)) $ zip targetList guessList
    biteCount = length (filter (`elem` targetList) guessList) - eatCount

-- | Generate all possible numbers with unique digits for specified size
allPossibleNumbers :: NumberSize -> [Number]
allPossibleNumbers numSize =
  let digitCount = numberSizeToInt numSize
  in concatMap (generateWithFirstDigit digitCount) [1..9]
  where
    generateWithFirstDigit :: Int -> Int -> [Number]
    generateWithFirstDigit n firstDigit =
      let combinations = generateRemainingCombinations (n - 1) [firstDigit]
      in map (\digits -> Number (map intToDigit (firstDigit : digits))) combinations
    
    generateRemainingCombinations :: Int -> [Int] -> [[Int]]
    generateRemainingCombinations 0 _ = [[]]
    generateRemainingCombinations n usedDigits =
      [ digit : rest
      | digit <- [0..9]
      , digit `notElem` usedDigits
      , rest <- generateRemainingCombinations (n - 1) (digit : usedDigits)
      ]

-- | Check if a number has valid format
isValidNumber :: NumberSize -> Number -> Bool
isValidNumber numSize (Number digits) =
  let expectedLength = numberSizeToInt numSize
      digitInts = map digitToInt digits
  in length digits == expectedLength &&
     length (nub digitInts) == expectedLength &&
     all (\d -> d >= 0 && d <= 9) digitInts &&
     (expectedLength == 0 || head digitInts /= 0)  -- First digit can't be 0

-- | Format number for display
formatNumber :: Number -> String
formatNumber = show

-- | Parse string to Number
parseNumber :: NumberSize -> String -> Maybe Number
parseNumber numSize str =
  let expectedLength = numberSizeToInt numSize
  in if length str == expectedLength && all isDigit str
     then let digits = map (read . (:[])) str :: [Int]
          in if length (nub digits) == expectedLength
             then mkNumber digits
             else Nothing
     else Nothing
  where
    isDigit :: Char -> Bool
    isDigit c = c `elem` ("0123456789" :: String)

-- | Check if hint indicates a win
isWinning :: NumberSize -> Hint -> Bool
isWinning numSize (Hint eats _) = eats == numberSizeToInt numSize