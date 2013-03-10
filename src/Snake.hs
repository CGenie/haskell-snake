-- Snake.hs: Snake-control logic

module Snake where

import Basic

import Data.List (nub, sort)

data SnakeState = SnakeState {
    position    :: [Point],    -- Snake's position is a list containing all its elements.
                               -- The first element of this list is snake's head.
    direction   :: Direction,
    len         :: Int
} deriving (Show, Eq)

-- increases snake's length by 1
increaseSnakeLength :: SnakeState -> SnakeState
increaseSnakeLength (SnakeState position direction len) = SnakeState position direction (len + 1)

-- moves snake according to it's direction
moveSnake :: SnakeState -> SnakeState
moveSnake (SnakeState position direction len) = SnakeState (newPosition) direction len
                               where
                                newPosition = take len
                                                   ((head position |+| directionToPoint direction) : position)
                                -- check snake's length, if newPosition contains too many elements, remove them

-- basically we check if there are 2 same elements in the 'position' list
checkCollision :: [Point] -> Bool
checkCollision snakePosition = outOfBoundary || selfCollision
                where
                    mini          = head $ sort snakePosition
                    maxi          = head $ reverse $ sort snakePosition
                    outOfBoundary = (x mini < 1) || (y mini < 1) ||
                                    (x maxi > numRectsX) || (y maxi > numRectsY)
                    selfCollision = (length (nub snakePosition)) /= (length snakePosition)

initialSnakePosition = [Point 6 6]
initialSnakeState = SnakeState initialSnakePosition North 1

snakeEatsApple :: [Point] -> Point -> Bool
snakeEatsApple snakePosition applePosition = applePosition `elem` snakePosition

changeSnakeDirection :: SnakeState -> Direction -> SnakeState
changeSnakeDirection snakeState dir
            | oppositeDirections dir currentDirection = snakeState -- don't allow to change
                                                                         -- to opposite direction
            | otherwise    = snakeState { direction = dir }
        where
            currentDirection = direction snakeState
