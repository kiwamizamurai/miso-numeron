{-# LANGUAGE DeriveGeneric #-}

module Game.Config
  ( NumberSize(..)
  , GameLevel(..)
  , GameConfig(..)
  , numberSizeToInt
  , defaultConfig
  , allNumberSizes
  , allGameLevels
  ) where

import GHC.Generics (Generic)

-- | Number of digits in the game
data NumberSize
  = Three  -- 3 digits
  | Four   -- 4 digits  
  | Five   -- 5 digits
  deriving (Show, Eq, Generic)

-- | Game difficulty levels
data GameLevel
  = Level1  -- Random guessing
  | Level2  -- Simple elimination
  | Level3  -- Optimal with performance fallback
  deriving (Show, Eq, Generic)

-- | Game configuration
data GameConfig = GameConfig
  { numberSize :: NumberSize
  , gameLevel :: GameLevel
  } deriving (Show, Eq, Generic)

-- | Convert NumberSize to Int
numberSizeToInt :: NumberSize -> Int
numberSizeToInt Three = 3
numberSizeToInt Four = 4
numberSizeToInt Five = 5

-- | Default configuration
defaultConfig :: GameConfig
defaultConfig = GameConfig Four Level2

-- | All available number sizes
allNumberSizes :: [NumberSize]
allNumberSizes = [Three, Four, Five]

-- | All available game levels
allGameLevels :: [GameLevel]
allGameLevels = [Level1, Level2, Level3]