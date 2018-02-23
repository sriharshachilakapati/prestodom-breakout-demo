module Breakout.Types where

-- | A type for the game state. Consists all the data in the game
type GameState =
  { bricks :: Array Entity
  , paddle :: Entity
  , ball :: Entity
  , ballSpeedX :: Int
  , ballSpeedY :: Int
  , keyLeft :: Boolean
  , keyRight :: Boolean
  , keySpace :: Boolean
  , launched :: Boolean
  , score :: Int
  , lives :: Int
  , currentScreen :: GameScreen
  }

-- | A general type representing the records of entities
type Entity =
  { x :: Int
  , y :: Int
  , w :: Int
  , h :: Int
  }

-- | A type enumeration that specifies which game state we are in
data GameScreen = PlayScreen | GameOverScreen | YouWinScreen

-- | A type used in PlayScreen while updating the bricks
type BrickUpdateResponse =
  { bricks :: Array Entity
  , state :: GameState
  }
