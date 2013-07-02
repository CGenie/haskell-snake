-- Snake.hs: Snake-control logic

module Snake where

import Basic

import Data.List (nub, sort)

data Snake = Snake {
    -- | Snake's position is a list containing all its elements.
    -- The first element of this list is snake's head.
    position    :: [Point],
    direction   :: Direction,
    -- | Snake's length
    len         :: Int
} deriving (Show, Eq)

-- | Increases snake's length by 1
increaseSnakeLength :: Snake -> Snake
increaseSnakeLength (Snake position direction len) = Snake position direction (len + 1)

-- | Moves snake according to it's direction
moveSnake :: Snake -> Snake
moveSnake (Snake position direction len) = Snake (newPosition) direction len
                               where
                                newPosition = take len
                                                   ((head position |+| directionToPoint direction) : position)
                                -- check snake's length, if newPosition contains too many elements, remove them

-- | Basically we check if there are 2 same elements in the 'position' list
checkCollision :: Snake -> Bool
checkCollision snake = outOfBoundary || selfCollision
                where
                    snakePosition = position snake
                    mini          = head $ sort snakePosition
                    maxi          = head $ reverse $ sort snakePosition
                    outOfBoundary = (x mini < 1) || (y mini < 1) ||
                                    (x maxi > numRectsX) || (y maxi > numRectsY)
                    selfCollision = (length (nub snakePosition)) /= (length snakePosition)

-- | Starting position & body
initialSnakePosition = [Point 6 6]
initialSnakePositionBottom = [Point 6 11]
initialSnake = Snake initialSnakePosition North 1
initialSnakeBottom = Snake initialSnakePositionBottom North 1

snakeEatsApple :: Snake -> Point -> Bool
snakeEatsApple snake apple = apple `elem` (position snake)

-- | Don't allow to change to opposite direction immediately
tryChangeSnakeDirection :: Direction -> Direction -> Direction
tryChangeSnakeDirection direction snakeDirection
            | oppositeDirections direction snakeDirection = snakeDirection
            | otherwise    = direction

changeSnakeDirection :: Snake -> Direction -> Snake
changeSnakeDirection snake dir = snake {direction = tryChangeSnakeDirection dir currentDirection}
        where
            currentDirection = direction snake
