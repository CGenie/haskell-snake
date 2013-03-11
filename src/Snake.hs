-- Snake.hs: Snake-control logic

module Snake where

import Basic

import Data.List (nub, sort)

data SnakeState = SnakeState {
    -- | Snake's position is a list containing all its elements.
    -- The first element of this list is snake's head.
    position    :: [Point],
    direction   :: Direction,
    -- | Snake's length
    len         :: Int
} deriving (Show, Eq)

-- | Increases snake's length by 1
increaseSnakeLength :: SnakeState -> SnakeState
increaseSnakeLength (SnakeState position direction len) = SnakeState position direction (len + 1)

-- | Moves snake according to it's direction
moveSnake :: SnakeState -> SnakeState
moveSnake (SnakeState position direction len) = SnakeState (newPosition) direction len
                               where
                                newPosition = take len
                                                   ((head position |+| directionToPoint direction) : position)
                                -- check snake's length, if newPosition contains too many elements, remove them

-- | Basically we check if there are 2 same elements in the 'position' list
checkCollision :: [Point] -> Bool
checkCollision snakePosition = outOfBoundary || selfCollision
                where
                    mini          = head $ sort snakePosition
                    maxi          = head $ reverse $ sort snakePosition
                    outOfBoundary = (x mini < 1) || (y mini < 1) ||
                                    (x maxi > numRectsX) || (y maxi > numRectsY)
                    selfCollision = (length (nub snakePosition)) /= (length snakePosition)

-- | Starting position & body
initialSnakePosition = [Point 6 6]
initialSnakeState = SnakeState initialSnakePosition North 1

snakeEatsApple :: [Point] -> Point -> Bool
snakeEatsApple snakePosition applePosition = applePosition `elem` snakePosition

-- | Don't allow to change to opposite direction immediately
changeSnakeDirection :: SnakeState -> Direction -> SnakeState
changeSnakeDirection snakeState dir
            | oppositeDirections dir currentDirection = snakeState
            | otherwise    = snakeState { direction = dir }
        where
            currentDirection = direction snakeState
