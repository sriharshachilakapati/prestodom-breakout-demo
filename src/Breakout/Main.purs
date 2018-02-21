module Breakout.Main where

import Breakout.PlayScreen (renderPlayScreen, updatePlayScreen)
import Breakout.Types (GameScreen(..), GameState)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Array (concatMap, (..))
import FRP (FRP)
import FRP.Behavior.Time (millisSinceEpoch)
import FRP.Event (subscribe)
import FRP.Event.Keyboard (down, up)
import FRP.Event.Time (animationFrame)
import Halogen.VDom (VDom)
import Halogen.VDom.DOM.Prop (Prop)
import Prelude (Unit, bind, discard, map, negate, pure, show, unit, ($), (*), (*>), (+), (<$>), (<>))
import PrestoDOM.Elements (imageView, linearLayout, relativeLayout, textView)
import PrestoDOM.Properties (background, cornerRadius, gravity, height, id_, imageUrl, margin, orientation, text, width)
import PrestoDOM.Util (getState, initializeState, patch, render, updateState)

-- Renders a key (used to give instructions to player) onto the screen
renderKey :: forall i p. String -> String -> Int -> VDom (Array (Prop i)) p
renderKey keyId key size =
  linearLayout
    [ id_ keyId
    , width $ show (size * 50)
    , height "50"
    , margin "5,5,5,5"
    , gravity "center"
    , background "#ddd"
    , cornerRadius "5"
    ]
    [ textView
        [ text key
        , width "match_parent"
        , height "20"
        , gravity "center"
        ]
    ]

-- | A function that renders a screen with text
renderTextScreen :: forall i p. String -> VDom (Array (Prop i)) p
renderTextScreen content =
  linearLayout
    [ id_ "empty"
    , width "match_parent"
    , height "match_parent"
    , gravity "center"
    ]
    [ textView
      [ width "match_parent"
      , height "20"
      , gravity "center"
      , text content
      ]
    ]

-- | The function that is responsible to render the game screen. Checks the current screen and calls the respective
-- | render function, one specific to that screen.
renderGameScreen :: forall i p. GameState -> VDom (Array (Prop i)) p
renderGameScreen state = case state.currentScreen of
  PlayScreen -> renderPlayScreen state
  GameOverScreen -> renderTextScreen "Alas! Game Over!"
  YouWinScreen -> renderTextScreen "Howdy! You win!"

-- | The game world. Usually you'll notice the word widget, but this is named as world as it contains all the game
-- | entities. This is the template of the whole screen.
world :: forall i p. GameState -> VDom (Array (Prop i)) p
world (state :: GameState) =
  linearLayout
    [ id_ "container"
    , width "match_parent"
    , height "match_parent"
    , gravity "center"
    , orientation "vertical"
    , background "#2d3436"
    ]
    [ relativeLayout
        [ id_ "contentScreen"
        , width "640"
        , height "480"
        ]
        [ imageView
            [ id_ "background"
            , width "match_parent"
            , height "match_parent"
            , imageUrl "resources/background"
            ]
        , renderGameScreen state
        , relativeLayout
            [ id_ "infoPanel"
            , width "match_parent"
            , height "match_parent"
            , margin "10,10,10,10"
            ]
            [ textView
                [ id_ "scoreLabel"
                , text ("Score: " <> (show state.score))
                ]
            , textView
                [ id_ "livesLabel"
                , text ("Lives: " <> (show state.lives))
                , width "match_parent"
                , height "match_parent"
                , gravity "right"
                ]
            ]
        ]
    , linearLayout
        [ id_ "controls"
        , width "640"
        , height "70"
        , gravity "center"
        , background "#6b967d"
        ]
        [ renderKey "left" "<" 1
        , renderKey "launch" "SPACE" 5
        , renderKey "right" ">" 1
        ]
    ]

-- | The entry point of the game. Here we initialize the state, create the entities, and starts rendering the game
main :: forall e. Eff (dom :: DOM, console :: CONSOLE, frp :: FRP | e) Unit
main = do
  -- Initialize the state to an empty object
  initializeState

  -- Reset the game to the start
  resetGame

  -- Get the updated state and start rendering the game
  state <- getState
  render (world state) listen

  pure unit

-- | Central place to update the whole game
updateGame :: forall e. Eff (console :: CONSOLE | e) Unit
updateGame = do
  (state :: GameState) <- getState

  -- Switch the game screen and update them on their own
  case state.currentScreen of
    PlayScreen -> updatePlayScreen
    _ -> if state.keySpace then resetGame else pure unit

-- | Resets the game to the starting state. Creates the entities, and initializes the game state to the default.
resetGame :: forall e. Eff (console :: CONSOLE | e) Unit
resetGame = do
  (state :: GameState) <- getState

  -- Create the entities in the game. We need a padde, a ball and some bricks.
  _ <- updateState "paddle" { x: 245, y: 420, w: 150, h: 20 }
  _ <- updateState "ball" { x: 250, y: 385, w: 20, h: 20 }
  _ <- updateState "bricks" $
        concatMap (\j ->
          map (\i -> { x: i * 70 + 10, y: j * 30 + 20, w: 60, h: 20 }) (1..7)) (1..5)

  -- Set the keys to false. We store the input here (because behaviors won't reflect key up or down)
  _ <- updateState "keyLeft" false
  _ <- updateState "keyRight" false
  _ <- updateState "keySpace" false
  _ <- updateState "launched" false

  -- The speeds of the ball. Make it go to top-right on the initial launch
  _ <- updateState "ballSpeedX" 4
  _ <- updateState "ballSpeedY" $ -4

  -- The score and lives in the game. Necessary for almost any game
  _ <- updateState "score" 0
  _ <- updateState "lives" 3

  -- The initial screen the game is in
  _ <- updateState "currentScreen" PlayScreen

  pure unit

-- | The eval function is the function that gets called whenever a UI event occurred. In our case, the only event we
-- | are calling this is with is the animationFrame event which repeatedly occurs when in browser animation frame is
-- | granted for us. And yes, this uses `window.requestAnimationFrame` under the hood.
eval :: forall e. Number -> Eff (console :: CONSOLE | e) GameState
eval _ = updateGame *> getState

-- This function sets up the events to the game and the behaviors. Once that is done, we start patching the dom
listen :: forall e. Eff (console :: CONSOLE, frp :: FRP | e) (Eff (frp :: FRP, console :: CONSOLE | e) Unit)
listen = do
  -- Setup keydown events
  _ <- down `subscribe` (\key -> case key of
          37 -> updateState "keyLeft" true
          39 -> updateState "keyRight" true
          32 -> updateState "keySpace" true
          _ -> getState)

  -- Setup keyup events
  _ <- up `subscribe` (\key -> case key of
          37 -> updateState "keyLeft" false
          39 -> updateState "keyRight" false
          32 -> updateState "keySpace" false
          _ -> getState)

  -- Start patching the dom
  let behavior = eval <$> millisSinceEpoch
  let events = (animationFrame)

  patch world behavior events
