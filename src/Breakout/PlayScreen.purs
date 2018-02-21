module Breakout.PlayScreen where

import Breakout.Types (Entity, GameScreen(..), GameState)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Array (filter, length)
import Data.Traversable (traverse)
import Halogen.VDom (VDom)
import Halogen.VDom.DOM.Prop (Prop)
import Prelude (Unit, bind, map, max, min, negate, not, pure, show, unit, ($), (&&), (*>), (+), (-), (/), (<=), (<>), (==), (>), (>=), (||))
import PrestoDOM.Elements (imageView, relativeLayout)
import PrestoDOM.Properties (height, id_, imageUrl, margin, width)
import PrestoDOM.Util (getState, updateState)

-- | Renders an entity onto the screen by creating a VDom element that represents the entity
renderEntity :: forall i p. String -> String -> Entity -> VDom (Array (Prop i)) p
renderEntity entityId imageSource entity =
  imageView
    [ id_ entityId
    , margin ((show entity.x) <> "," <> (show entity.y) <> ",0,0")
    , width $ show entity.w
    , height $ show entity.h
    , imageUrl imageSource
    ]

-- | A helper function for rendering bricks
renderBrick :: forall i p. Entity -> VDom (Array (Prop i)) p
renderBrick brick = renderEntity ("brick" <> (show brick.x) <> (show brick.y)) "resources/brick" brick

-- | A helper function for rendering the paddle
renderPaddle :: forall i p. Entity -> VDom (Array (Prop i)) p
renderPaddle = renderEntity "paddle" "resources/bat"

-- | A helper function for rendering the ball
renderBall :: forall i p. Entity -> VDom (Array (Prop i)) p
renderBall = renderEntity "ball" "resources/ball"

-- | A utility function that renders the game onto the screen
renderPlayScreen :: forall i p. GameState -> VDom (Array (Prop i)) p
renderPlayScreen state =
  relativeLayout
    [ id_ "world"
    , width "match_parent"
    , height "match_parent"
    ]
    [ renderPaddle state.paddle
    , renderBall state.ball
    , relativeLayout
        [ id_ "bricks"
        , width "match_parent"
        , height "match_parent"
        ]
        (map renderBrick state.bricks)
    ]

-- | A helper function that says if two entities intersect each other. We treat the entities as rectangles internally,
-- | and hence we can use the minified SAT (Separating Axis Theorem) to find if two entities overlapped.
intersects :: Entity -> Entity -> Boolean
intersects a b = not (a.x > b.x + b.w || b.x > a.x + a.w) &&
                 not (a.y > b.y + b.h || b.y > a.y + a.h)

-- | A helper function that says how much has an entity `a` penetrated into an entity `b` on the x-axis.
getIntersectionWidth :: Entity -> Entity -> Int
getIntersectionWidth a b = if ax2 > bx2 then bx2 - ax1 else ax2 - bx1
  where
    ax1 = a.x
    bx1 = b.x
    ax2 = a.x + a.w
    bx2 = b.x + b.w

-- | A helper function that says how much has an entiay `a` penetrated into an entiay `b` on the y-axis.
getIntersectionHeight :: Entity -> Entity -> Int
getIntersectionHeight a b = if ay2 > by2 then by2 - ay1 else ay2 - by1
  where
    ay1 = a.y
    by1 = b.y
    ay2 = ay1 + a.h
    by2 = by1 + b.h

-- | A function that bounces the ball off another entity in the game.
bounce :: forall e. Entity -> Eff (console :: CONSOLE | e) Unit
bounce other = do
  (state :: GameState) <- getState

  -- Get the penetration area between the two entities
  let iw = getIntersectionWidth state.ball other
  let ih = getIntersectionHeight state.ball other

  -- Depending the penetration depth, alter the speeds of the ball and update the state
  _ <- if iw >= ih then updateState "ballSpeedY" $ - state.ballSpeedY else getState
  _ <- if iw <= ih then updateState "ballSpeedX" $ - state.ballSpeedX else getState

  -- Move the ball to the vertical position just before the collision occurs for accurate response
  _ <- if iw >= ih then
        if state.ball.y > other.y then
          updateState "ball" state.ball { y = state.ball.y + ih }
        else
          updateState "ball" state.ball { y = state.ball.y - ih }
       else getState

  -- Move the ball to the horizontal position just before the collision occurs for accurate response
  _ <- if iw <= ih then
        if state.ball.x > other.x then
          updateState "ball" state.ball { x = state.ball.x - iw }
        else
          updateState "ball" state.ball { x = state.ball.x + iw }
      else getState

  pure unit

-- | A function that checks collision between a brick and the ball. If intersects, it tries to bounce the ball off the
-- | brick. It also returns the updated brick record stating the updated dead property
checkBrickCollision :: forall e. Entity -> Eff (console :: CONSOLE | e) Entity
checkBrickCollision brick = do
  (state :: GameState) <- getState

  -- Destroy the brick if it hits the ball, increase the score and bounce the ball
  if intersects brick state.ball then
    updateState "score" (state.score + 1) *> bounce brick *> pure brick { dead = true }
    else
      pure brick { dead = false }

-- | This function is the place where the game is updated. We move the paddle, the ball, check for intersections, etc.,
updatePlayScreen :: forall e. Eff (console :: CONSOLE | e) Unit
updatePlayScreen = do
  (state :: GameState) <- getState

  -- If the left key is pressed, then move the paddle to the left
  _ <- if state.keyLeft then
          updateState "paddle" state.paddle { x = state.paddle.x - 5 }
        else getState

  -- If the right key is pressed, then move the paddle to the right
  _ <- if state.keyRight then
          updateState "paddle" state.paddle { x = state.paddle.x + 5 }
        else getState

  -- Clamp the paddle positions so that it won't leave the game area
  _ <- updateState "paddle" state.paddle { x = (max (min state.paddle.x $ 640 - state.paddle.w) 0) }

  -- Launch the ball on space key
  _ <- updateState "launched" $ state.launched || state.keySpace

  _ <- if state.launched == false then
          -- If the ball hasn't been launched, move it along with the paddle
          updateState "ball" state.ball
            { x = state.paddle.x + state.paddle.w / 2 - state.ball.w / 2
            , y = state.paddle.y - 30
            }
        else
          -- The ball has been launched already. Move it according to it's own speed
          updateState "ball" state.ball
            { x = state.ball.x + state.ballSpeedX
            , y = state.ball.y + state.ballSpeedY
            }

  -- Bounce the ball horizontally if hit to walls of the game screen
  _ <- if state.ball.x <= 0 || state.ball.x + state.ball.w >= 640 then
          updateState "ballSpeedX" $ - state.ballSpeedX
        else getState

  -- Bounce the ball vertically if it hits the top wall of the screen
  _ <- if state.ball.y <= 0 then
          updateState "ballSpeedY" $ - state.ballSpeedY
        else getState

  -- Bounce the ball if it hits the paddle
  _ <- if intersects state.ball state.paddle then bounce state.paddle else pure unit

  -- If the ball hits the bottom wall of the screen, you die. Reset the position, and decrease a life
  _ <- if state.ball.y + state.ball.h >= 480 then do
          _ <- updateState "launched" false
          _ <- updateState "ballSpeedY" $ -4
          _ <- updateState "lives" $ state.lives - 1
          pure unit
        else pure unit

  -- Traverse all the bricks checking for collisions with the ball.
  updatedBricks <- traverse checkBrickCollision state.bricks

  -- Filter away all the dead bricks from the list of bricks in the game state
  _ <- updateState "bricks" $ filter (\brick -> brick.dead == false) updatedBricks

  -- If all the bricks are finished, go to the win screen
  _ <- if length state.bricks == 0 then
          updateState "currentScreen" YouWinScreen
        else getState

  -- If the lives became 0, go to the game over screen
  _ <- if state.lives == 0 then
          updateState "currentScreen" GameOverScreen
        else getState

  pure unit
