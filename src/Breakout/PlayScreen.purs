module Breakout.PlayScreen where

import Breakout.Types (Entity, GameScreen(..), GameState, BrickUpdateResponse)
import Data.Array (length, snoc, (!!))
import Data.Maybe (Maybe(Just, Nothing))
import Prelude (map, max, min, negate, not, show, ($), (&&), (+), (-), (/), (<=), (<>), (==), (>), (>=), (>>>), (||))
import PrestoDOM.Core (PrestoDOM)
import PrestoDOM.Elements (imageView, relativeLayout)
import PrestoDOM.Properties (height, id_, imageUrl, margin, width)
import PrestoDOM.Types (Length(..))

-- | Renders an entity onto the screen by creating a VDom element that represents the entity
renderEntity :: forall i p. String -> String -> Entity -> PrestoDOM i p
renderEntity entityId imageSource entity =
  imageView
    [ id_ entityId
    , margin ((show entity.x) <> "," <> (show entity.y) <> ",0,0")
    , width $ V entity.w
    , height $ V entity.h
    , imageUrl imageSource
    ]

-- | A helper function for rendering bricks
renderBrick :: forall i p. Entity -> PrestoDOM i p
renderBrick brick = renderEntity ("brick" <> (show brick.x) <> (show brick.y)) "resources/brick" brick

-- | A helper function for rendering the paddle
renderPaddle :: forall i p. Entity -> PrestoDOM i p
renderPaddle = renderEntity "paddle" "resources/bat"

-- | A helper function for rendering the ball
renderBall :: forall i p. Entity -> PrestoDOM i p
renderBall = renderEntity "ball" "resources/ball"

-- | A utility function that renders the game onto the screen
renderPlayScreen :: forall i p. GameState -> PrestoDOM i p
renderPlayScreen state =
  relativeLayout
    [ id_ "world"
    , width Match_Parent
    , height Match_Parent
    ]
    [ renderPaddle state.paddle
    , renderBall state.ball
    , relativeLayout
        [ id_ "bricks"
        , width Match_Parent
        , height Match_Parent
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
bounce :: GameState -> Entity -> GameState
bounce state other = do
  -- Get the penetration area between the two entities
  let iw = getIntersectionWidth state.ball other
  let ih = getIntersectionHeight state.ball other

  -- Change the ball speed to bounce and correct the ball position so that it doesn't get stuck inside brick or paddle
  changeBallSpeed iw ih $ correctBallPosition iw ih state

  where
    -- Depending the penetration depth, alter the speeds of the ball and update the state
    changeBallSpeed :: Int -> Int -> GameState -> GameState
    changeBallSpeed iw ih s = changeVerticalSpeed iw ih $ changeHorizontalSpeed iw ih s

    -- If the width of intersection rectangle is greater than the height, it is a collision on either top or bottom of ball
    changeVerticalSpeed :: Int -> Int -> GameState -> GameState
    changeVerticalSpeed iw ih s = if iw >= ih then s { ballSpeedY = - s.ballSpeedY } else s

    -- If the height of intersection rectangle is greater than the width, it is a collision on either left or right of ball
    changeHorizontalSpeed :: Int -> Int -> GameState -> GameState
    changeHorizontalSpeed iw ih s = if iw <= ih then s { ballSpeedX = - s.ballSpeedX } else s

    -- Move the ball position to the moment before the collision occurred
    correctBallPosition :: Int -> Int -> GameState -> GameState
    correctBallPosition iw ih s = correctVerticalPosition iw ih $ correctHorizontalPosition iw ih s

    -- Move the ball to the vertical position just before the collision occurs for accurate response
    correctVerticalPosition :: Int -> Int -> GameState -> GameState
    correctVerticalPosition iw ih s =
      if iw >= ih then
        if s.ball.y > other.y then
          s { ball = s.ball { y = s.ball.y + ih } }
        else
          s { ball = s.ball { y = s.ball.y - ih } }
      else s

    -- Move the ball to the horizontal position just before the collision occurs for accurate response
    correctHorizontalPosition :: Int -> Int -> GameState -> GameState
    correctHorizontalPosition iw ih s =
      if iw <= ih then
        if s.ball.x > other.x then
          s { ball = s.ball { x = s.ball.x - iw } }
        else
          s { ball = s.ball { x = s.ball.x + iw } }
      else s

-- | This function is the place where the game is updated. We move the paddle, the ball, check for intersections, etc.,
updatePlayScreen :: GameState -> GameState
updatePlayScreen =
  (updatePaddle >>> updateBall >>> checkCollisions >>> checkWinCondition >>> checkGameOverCondition)

  where
    updatePaddle :: GameState -> GameState
    updatePaddle = checkMovePaddleLeft >>> checkMovePaddleRight >>> clampPaddleToScreen

    -- If the left key is pressed, move the paddle to the left
    checkMovePaddleLeft :: GameState -> GameState
    checkMovePaddleLeft s = if s.keyLeft then s { paddle = s.paddle { x = s.paddle.x - 5 } } else s

    -- If the right key is pressed, move the paddle to the right
    checkMovePaddleRight :: GameState -> GameState
    checkMovePaddleRight s = if s.keyRight then s { paddle = s.paddle { x = s.paddle.x + 5 } } else s

    -- Don't let the paddle move out of the game screen! Clamp it's position to the screen boundaries
    clampPaddleToScreen :: GameState -> GameState
    clampPaddleToScreen s = s { paddle = s.paddle { x = (max (min s.paddle.x $ 640 - s.paddle.w) 0) } }

    updateBall :: GameState -> GameState
    updateBall = checkLaunchBall >>> moveBall

    -- Launch the ball if space key is pressed
    checkLaunchBall :: GameState -> GameState
    checkLaunchBall s = s { launched = s.launched || s.keySpace }

    -- If the ball is launched, apply the speed to the ball, else stick it to the paddle
    moveBall :: GameState -> GameState
    moveBall s = if s.launched then applySpeedToBall s else stickBallToPaddle s

    stickBallToPaddle :: GameState -> GameState
    stickBallToPaddle s = s { ball = s.ball
                                { x = s.paddle.x + s.paddle.w / 2 - s.ball.w / 2
                                , y = s.paddle.y - 30
                                }
                            }

    applySpeedToBall :: GameState -> GameState
    applySpeedToBall s = s { ball = s.ball { x = s.ball.x + s.ballSpeedX, y = s.ball.y + s.ballSpeedY }}

    -- Check the game for collisions and respond to them
    checkCollisions :: GameState -> GameState
    checkCollisions = checkWallCollisions >>> checkPaddleCollision >>> checkBricksCollision

    -- Check the collisions between ball and walls
    checkWallCollisions :: GameState -> GameState
    checkWallCollisions = checkSideWallCollisions >>> checkTopWallCollision >>> checkBottomWallCollision

    -- Bounce ball horizontally if collision with side walls
    checkSideWallCollisions :: GameState -> GameState
    checkSideWallCollisions s =
      if s.ball.x <= 0 || s.ball.x + s.ball.w >= 640 then s { ballSpeedX = - s.ballSpeedX } else s

    -- Bounce ball vertically if collision with top wall
    checkTopWallCollision :: GameState -> GameState
    checkTopWallCollision s = if s.ball.y <= 0 then s { ballSpeedY = - s.ballSpeedY } else s

    -- You lose a life if the ball collides the bottom wall
    checkBottomWallCollision :: GameState -> GameState
    checkBottomWallCollision s =
      if s.ball.y + s.ball.h >= 480 then s { launched = false, ballSpeedY = -4, lives = s.lives - 1 } else s

    -- Bounce the ball if it collides with the paddle
    checkPaddleCollision :: GameState -> GameState
    checkPaddleCollision s = if intersects s.ball s.paddle then bounce s s.paddle else s

    -- Process the bricks and check collisions with the ball
    checkBricksCollision :: GameState -> GameState
    checkBricksCollision s = do
      let { bricks, state } = updateBricks { bricks: [], state: s } 0
      state { bricks = bricks }

    -- This is a helper function that is used to recursively iterate over the bricks and update the state
    updateBricks :: BrickUpdateResponse -> Int -> BrickUpdateResponse
    updateBricks { bricks, state } brickIndex =
      case state.bricks !! brickIndex of

        -- If there is no brick at this index, we returned the end! Return the response
        Nothing -> { bricks: bricks, state: state }

        -- If there is a brick indeed, we need to process it in the game
        Just brick ->
          if intersects brick state.ball then do
            -- Bounce the ball on the brick and update the score
            let newState = (bounce state brick) { score = state.score + 1 }

            -- This brick is destroyed, so don't add it to the array and continue with next brick
            updateBricks { bricks: bricks, state: newState } (brickIndex + 1)
          else
            -- There is no intersection with this brick, add it to the array and continue with next brick
            updateBricks { bricks: bricks `snoc` brick, state: state } (brickIndex + 1)

    -- If there are no bricks remaining in the screen, then the player wins
    checkWinCondition :: GameState -> GameState
    checkWinCondition s = if length s.bricks == 0 then s { currentScreen = YouWinScreen } else s

    -- If there are no lives left, the player is dead and the game is over!
    checkGameOverCondition :: GameState -> GameState
    checkGameOverCondition s = if s.lives == 0 then s { currentScreen = GameOverScreen } else s
