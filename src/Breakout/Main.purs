module Breakout.Main where

import Breakout.PlayScreen (renderPlayScreen, updatePlayScreen)
import Breakout.Types (GameScreen(..), GameState)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Array (concatMap, (..))
import FRP (FRP)
import FRP.Behavior.Keyboard (key)
import FRP.Event.Time (animationFrame)
import Prelude (Unit, bind, map, negate, pure, show, unit, ($), (*), (*>), (+), (<$>), (<>), (<*>))
import PrestoDOM.Core (PrestoDOM)
import PrestoDOM.Elements (imageView, linearLayout, relativeLayout, textView)
import PrestoDOM.Properties (background, cornerRadius, gravity, height, id_, imageUrl, margin, orientation, text, width)
import PrestoDOM.Types (Length(..))
import PrestoDOM.Util (render)

-- Renders a key (used to give instructions to player) onto the screen
renderKey :: forall i p. String -> String -> Int -> PrestoDOM i p
renderKey keyId key size =
  linearLayout
    [ id_ keyId
    , width $ V (size * 50)
    , height (V 50)
    , margin "5,5,5,5"
    , gravity "center"
    , background "#ddd"
    , cornerRadius "5"
    ]
    [ textView
        [ text key
        , width Match_Parent
        , height (V 20)
        , gravity "center"
        ]
    ]

-- | A function that renders a screen with text
renderTextScreen :: forall i p. String -> PrestoDOM i p
renderTextScreen content =
  linearLayout
    [ id_ "empty"
    , width Match_Parent
    , height Match_Parent
    , gravity "center"
    ]
    [ textView
      [ width Match_Parent
      , height (V 20)
      , gravity "center"
      , text content
      ]
    ]

-- | The function that is responsible to render the game screen. Checks the current screen and calls the respective
-- | render function, one specific to that screen.
renderGameScreen :: forall i p. GameState -> PrestoDOM i p
renderGameScreen state = case state.currentScreen of
  PlayScreen -> renderPlayScreen state
  GameOverScreen -> renderTextScreen "Alas! Game Over!"
  YouWinScreen -> renderTextScreen "Howdy! You win!"

-- | The game world. Usually you'll notice the word widget, but this is named as world as it contains all the game
-- | entities. This is the template of the whole screen.
world :: forall i p. GameState -> PrestoDOM i p
world (state :: GameState) =
  linearLayout
    [ id_ "container"
    , width Match_Parent
    , height Match_Parent
    , gravity "center"
    , orientation "vertical"
    , background "#2d3436"
    ]
    [ relativeLayout
        [ id_ "contentScreen"
        , width (V 640)
        , height (V 480)
        ]
        [ imageView
            [ id_ "background"
            , width Match_Parent
            , height Match_Parent
            , imageUrl "resources/background"
            ]
        , renderGameScreen state
        , relativeLayout
            [ id_ "infoPanel"
            , width Match_Parent
            , height Match_Parent
            , margin "10,10,10,10"
            ]
            [ textView
                [ id_ "scoreLabel"
                , text ("Score: " <> (show state.score))
                ]
            , textView
                [ id_ "livesLabel"
                , text ("Lives: " <> (show state.lives))
                , width Match_Parent
                , height Match_Parent
                , gravity "right"
                ]
            ]
        ]
    , linearLayout
        [ id_ "controls"
        , width (V 640)
        , height (V 70)
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
    let initialState = resetGame
    { stateBeh, updateState } <- render world initialState
    updateState (eval <$> key 37 <*> key 39 <*> key 32 <*> stateBeh) animationFrame *>
    pure unit

-- | Central place to update the whole game
updateGame :: GameState -> GameState
updateGame state = do
  -- Switch the game screen and update them on their own
  case state.currentScreen of
    PlayScreen -> updatePlayScreen state
    _ -> if state.keySpace then resetGame else state

-- | Resets the game to the starting state. Creates the entities, and initializes the game state to the default.
resetGame :: GameState
resetGame =
  { -- Create the entities in the game. We need a padde, a ball and some bricks.
    bricks: concatMap (\j ->
              map (\i -> { x: i * 70 + 10, y: j * 30 + 20, w: 60, h: 20 }) (1..7)) (1..5)
  , paddle: { x: 245, y: 420, w: 150, h: 20 }
  , ball: { x: 250, y: 385, w: 20, h: 20 }

  -- The speeds of the ball. Make it go to top-right on the initial launch
  , ballSpeedX: 4
  , ballSpeedY: -4

  -- Set the keys to false. We store the input here (because behaviors won't reflect key up or down)
  , keyLeft: false
  , keyRight: false
  , keySpace: false
  , launched: false

  -- The score and lives in the game. Necessary for almost any game
  , score: 0
  , lives: 3

  -- The initial screen the game is in
  , currentScreen: PlayScreen
  }

-- | The eval function is the function that gets called whenever a UI event occurred. In our case, the only event we
-- | are calling this is with is the animationFrame event which repeatedly occurs when in browser animation frame is
-- | granted for us. And yes, this uses `window.requestAnimationFrame` under the hood.
eval :: Boolean -> Boolean -> Boolean -> GameState -> GameState
eval keyLeft keyRight keySpace state =
  updateGame state { keyLeft = keyLeft, keyRight = keyRight, keySpace = keySpace }
