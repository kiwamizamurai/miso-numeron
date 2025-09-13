-- | Level 3: Optimal information gain strategy with performance fallback
module Game.Strategy.Level3
  ( chooseOptimalGuess
  ) where

import Data.List (groupBy, minimumBy, sortBy)
import Data.Ord (comparing)
import Game.Types
import Game.Number (calculateHint)
import Game.State

-- | Hybrid strategy - Information gain for small sets, simple elimination for large sets
chooseOptimalGuess :: [Number] -> [GuessHistory] -> Number
chooseOptimalGuess candidates history =
  let candidateGuesses = filter (`notInHistory` history) candidates
      guessesToConsider = if null candidateGuesses then candidates else candidateGuesses
      -- Use simple strategy when candidate count is too high to avoid O(n²) explosion
      -- Threshold of 1000 keeps computation under ~1M operations
      threshold = 1000
  in if length candidates > threshold
       then head guessesToConsider  -- Fallback to Level 2 strategy
       else minimumBy (comparing (expectedRemaining candidates)) guessesToConsider
  where
    notInHistory num hist = not $ any (\(GuessHistory pastGuess _) -> pastGuess == num) hist

-- | Calculate expected remaining possibilities after a guess
expectedRemaining :: [Number] -> Number -> Double
expectedRemaining candidates candidate =
  let hintGroups = groupByHint candidates candidate
      groupSizes = map length hintGroups
      totalCandidates = fromIntegral (length candidates)
  in sum (map (\size -> (fromIntegral size / totalCandidates) * fromIntegral size) groupSizes)

-- | Group candidates by the hint they would produce with a guess
groupByHint :: [Number] -> Number -> [[Number]]
groupByHint candidates candidate =
  let candidatesWithHints = map (\c -> (c, calculateHint c candidate)) candidates
      sortedByHint = sortBy (comparing snd) candidatesWithHints
      groupedByHint = groupBy (\(_, h1) (_, h2) -> h1 == h2) sortedByHint
  in map (map fst) groupedByHint