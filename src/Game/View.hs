{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Game.View
  ( viewModel
  ) where

import Miso
import Miso.Html
import Miso.Html.Property (type_, value_, placeholder_)
import Miso.String (ms, MisoString, fromMisoString)
import Miso.Svg hiding (text)
import qualified Miso.Svg as Svg
import Miso.Svg.Property
import Data.Text (Text)
import qualified Data.Text as T

import Game.Types
import Game.Config
import Game.Model
import Game.Actions
import Game.Number (formatNumber, allPossibleNumbers)
import Game.State (possibilities)

-- | Main view function
viewModel :: AppState -> View AppState Action
viewModel state = div_ []
  [ h1_ [] [text "🎯 Numeron Game"]
  , if showConfig state
    then configPanel state
    else gameView state
  ]

-- | Configuration panel
configPanel :: AppState -> View model Action
configPanel AppState{..} = div_ []
  [ h2_ [] [text "Game Settings"]
  , div_ []
    [ h3_ [] [text $ ms $ "Number Size: " <> T.pack (show $ numberSizeToInt $ numberSize config) <> " digits"]
    , div_ [] $ map (sizeButton $ numberSize config) allNumberSizes
    ]
  , div_ []
    [ h3_ [] [text $ ms $ "Difficulty Level: " <> levelName (gameLevel config)]
    , div_ [] $ map (levelButton $ gameLevel config) allGameLevels
    ]
  , br_ []
  , button_ [onClick StartGameWithConfig] [text "🎮 Start Game"]
  , text " "
  , button_ [onClick ResetGame] [text "🎲 Quick Start (Default)"]
  ]
  where
    sizeButton current size = 
      let buttonText = if size == current 
                       then "✓ " <> show (numberSizeToInt size) <> " digits"
                       else show (numberSizeToInt size) <> " digits"
      in button_ [onClick (SetNumberSize size)]
         [text $ ms buttonText]
    
    levelButton current level =
      let levelText = levelName level
          buttonText = if level == current
                       then "✓ " <> T.unpack levelText
                       else T.unpack levelText
      in button_ [onClick (SetGameLevel level)] [text $ ms buttonText]
    
    levelName level = case level of
      Level1 -> "Easy"
      Level2 -> "Normal"
      Level3 -> "Hard"

-- | Main game view
gameView :: AppState -> View model Action
gameView state@AppState{..} = div_ []
  [ gameModeDisplay config
  , messageDisplay message
  , inputSection state
  , gameBoard state
  , button_ [onClick ToggleConfig] [text "⚙️ Settings"]
  ]
  
-- | Display current game mode
gameModeDisplay :: GameConfig -> View model Action
gameModeDisplay GameConfig{..} = div_ []
  [ text $ ms $ "📏 " <> T.pack (show $ numberSizeToInt numberSize) <> " digits | "
  , text $ ms $ "🎯 " <> levelText <> " mode"
  ]
  where
    levelText = case gameLevel of
      Level1 -> "Easy"
      Level2 -> "Normal"
      Level3 -> "Hard"

-- | Message display
messageDisplay :: Text -> View model Action
messageDisplay msg = div_ [] [text $ ms msg]

-- | Input section
inputSection :: AppState -> View model Action
inputSection AppState{..} = div_ []
  [ input_
    [ type_ "text"
    , value_ (ms currentInput)
    , onInput (InputChanged . T.pack . fromMisoString)
    , placeholder_ $ ms $ "Enter " <> T.pack (show $ numberSizeToInt $ numberSize config) <> " digits..."
    ]
  , text " "
  , submitButton
  , text " "
  , button_ [onClick ResetGame] [text "🔄 New Game"]
  ]
  where
    submitButton = case playerSecret of
      Nothing -> button_ [onClick SetPlayerSecret] [text "🔒 Set Secret"]
      Just _ -> button_ [onClick SubmitGuess] [text "📤 Submit"]

-- | Game board showing both player and CPU guesses
gameBoard :: AppState -> View model Action
gameBoard AppState{..} = div_ []
  [ turnIndicator currentTurn
  , div_ []
    [ div_ []
      [ h3_ [] [text "👤 Your Guesses"]
      , if null playerHistory
        then p_ [] [text "Make your guess!"]
        else ul_ [] $ map guessItem (reverse playerHistory)
      ]
    , div_ []
      [ h3_ [] [text "🤖 CPU Guesses"]
      , candidatesCounter $ length $ possibilities cpuState
      , if null cpuHistory
        then p_ [] [text "CPU is waiting..."]
        else ul_ [] $ map guessItem (reverse cpuHistory)
      , candidateGraph candidateHistory (numberSize config)
      ]
    ]
  , secretDisplay playerSecret
  ]

-- | Turn indicator
turnIndicator :: Player -> View model Action
turnIndicator turn = div_ []
  [ text $ case turn of
      Human -> "👤 Your turn"
      CPU -> "🤖 CPU is thinking..."
  ]

-- | Display a guess history item
guessItem :: GuessHistory -> View model Action
guessItem (GuessHistory num hint) = li_ []
  [ text $ ms $ T.pack (formatNumber num) <> ": "
  , span_ [] [text $ ms $ T.pack (show $ eat hint) <> " EAT"]
  , text ", "
  , span_ [] [text $ ms $ T.pack (show $ bite hint) <> " BITE"]
  ]

-- | Display secret number (for debugging or game end)
secretDisplay :: Maybe Number -> View model Action
secretDisplay Nothing = text ""
secretDisplay (Just secret) = div_ []
  [ text $ ms $ "Your secret: " <> T.pack (formatNumber secret)
  ]

-- | Display candidates counter
candidatesCounter :: Int -> View model Action
candidatesCounter count = div_ []
  [ text $ ms $ "📊 " <> T.pack (show count) <> " candidates"
  ]

-- | Display candidate history graph using SVG
candidateGraph :: [Int] -> NumberSize -> View model Action
candidateGraph history numSize = 
  if length history <= 1
  then text ""
  else 
    let maxCandidates = length $ allPossibleNumbers numSize
        width = 320 :: Double
        height = 180 :: Double
        padding = 30 :: Double
        graphWidth = width - 2 * padding - 10
        graphHeight = height - 2 * padding - 10
        
        -- Create points for the line graph
        numPoints = length history
        xStep = graphWidth / fromIntegral (max 1 (numPoints - 1))
        
        points = zipWith makePoint [0..] history
        makePoint i val = 
          let x = padding + fromIntegral i * xStep
              y = padding + graphHeight * (1 - fromIntegral val / fromIntegral maxCandidates)
          in (x, y)
        
        -- Create SVG path
        pathData = "M " <> T.intercalate " L " 
          [T.pack (show x) <> "," <> T.pack (show y) | (x, y) <- points]
        
    in div_ []
      [ h4_ [] [text "📈 Candidate Reduction"]
      , svg_ 
        [ width_ $ ms $ T.pack $ show width
        , height_ $ ms $ T.pack $ show height
        , viewBox_ $ ms $ "0 0 " <> T.pack (show width) <> " " <> T.pack (show height)
        ]
        [ -- Background
          rect_ 
            [ x_ "0", y_ "0"
            , width_ $ ms $ T.pack $ show width
            , height_ $ ms $ T.pack $ show height
            , fill_ "white"
            , stroke_ "gray"
            ]
          
          -- Grid lines
        , g_ [] $ map (\i -> 
            let y = padding + fromIntegral i * graphHeight / 4
            in line_ 
              [ x1_ $ ms $ T.pack $ show padding
              , y1_ $ ms $ T.pack $ show y
              , x2_ $ ms $ T.pack $ show (width - padding)
              , y2_ $ ms $ T.pack $ show y
              , stroke_ "lightgray"
              , strokeDasharray_ "2,2"
              ]
          ) [0..4]
          
          -- The line graph
        , Svg.path_ 
            [ d_ $ ms pathData
            , fill_ "none"
            , stroke_ "blue"
            , strokeWidth_ "2"
            ]
          
          -- Points
        , g_ [] $ map (\(x, y) ->
            circle_ 
              [ cx_ $ ms $ T.pack $ show x
              , cy_ $ ms $ T.pack $ show y
              , r_ "3"
              , fill_ "blue"
              ]
          ) points
          
          -- Y-axis labels
        , g_ [] $ map (\i ->
            let y = padding + fromIntegral i * graphHeight / 4
                val = round $ fromIntegral maxCandidates * (1 - fromIntegral i / 4)
            in Svg.text_
              [ x_ $ ms $ T.pack $ show (padding - 5)
              , y_ $ ms $ T.pack $ show (y + 3)
              , textAnchor_ "end"
              , fontSize_ "10"
              , fill_ "black"
              ] [text $ ms $ T.pack $ show val]
          ) [0..4]
          
          -- X-axis labels
        , g_ [] $ zipWith (\i _ ->
            Svg.text_
              [ x_ $ ms $ T.pack $ show (padding + fromIntegral i * xStep)
              , y_ $ ms $ T.pack $ show (height - padding + 15)
              , textAnchor_ "middle"
              , fontSize_ "10"
              , fill_ "black"
              ] [text $ ms $ T.pack $ show (i + 1)]
          ) [0..] history
          
          -- Axis labels
        , Svg.text_ 
            [ x_ $ ms $ T.pack $ show (width / 2)
            , y_ $ ms $ T.pack $ show (height - 2)
            , textAnchor_ "middle"
            , fontSize_ "10"
            ] [text "Turns"]
        , Svg.text_
            [ x_ $ ms $ T.pack $ show 5
            , y_ $ ms $ T.pack $ show (height / 2)
            , textAnchor_ "middle"
            , fontSize_ "10"
            , transform_ $ ms $ "rotate(-90 5 " <> T.pack (show (height / 2)) <> ")"
            ] [text "Candidates"]
        ]
      ]