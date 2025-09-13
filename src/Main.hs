{-# LANGUAGE CPP #-}

module Main where

import Miso
import Data.Maybe (fromJust)

import Game.Model (AppState(..), initialState)
import Game.Actions (Action(..))
import Game.Update (updateModel)
import Game.View (viewModel)
import Game.Types (mkNumber)
import Game.Config (NumberSize(..), numberSizeToInt)

#if !defined(__GHCJS__) && !defined(wasm32_HOST_ARCH)
import Language.Javascript.JSaddle.Warp as JSaddle
import Control.Monad.IO.Class (liftIO)
#endif

-- | WASM export
#ifdef wasm32_HOST_ARCH
foreign export javascript "hs_start" main :: IO ()
#endif

-- | Entry point for miso application  
main :: IO ()
main = 
#if !defined(__GHCJS__) && !defined(wasm32_HOST_ARCH)
  -- For JSaddle development server
  JSaddle.run 8080 $ do
    liftIO $ putStrLn "Running on http://localhost:8080"
    startApp app
#else
  -- For WASM and GHCJS builds
  run (startApp app)
#endif


-- | Main app configuration
app :: App AppState Action
app = component initStateWithSecret updateModel viewModel
  where
    -- Initialize with a pseudo-random CPU secret for WASM builds
    initStateWithSecret = initialState 
      { cpuSecret = Just (fromJust $ mkNumber [1,2,3,4]) 
      }