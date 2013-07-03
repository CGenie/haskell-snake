-- Snake.hs: Snake-control logic
{-# LANGUAGE TemplateHaskell #-}

module Snake where

import Control.Lens

import Basic

import Data.List (nub, sort)

data Snake = Snake {
    -- | Snake's position is a list containing all its elements.
    -- The first element of this list is snake's head.
    _position    :: [Point],
    _direction   :: Direction,
    -- | Snake's length
    _len         :: Int
} deriving (Show, Eq)

makeLenses ''Snake

-- | Increases snake's length by 1
increaseSnakeLength :: Snake -> Snake
increaseSnakeLength snake = len +~ 1 $ snake

-- | Moves snake according to it's direction
moveSnake :: Snake -> Snake
moveSnake snake = position .~ newPosition $ snake
             where
                -- |check snake's length, if newPosition contains too many elements, remove them
                newPosition = take (snake^.len)
                    ((head (snake^.position) |+| directionToPoint (snake^.direction)) :
                     (snake^.position))

-- | Basically we check if there are 2 same elements in the 'position' list
checkCollision :: Snake -> Bool
checkCollision snake = outOfBoundary || selfCollision
                where
                    snakePosition = snake^.position
                    mini          = head $ sort snakePosition
                    maxi          = head $ reverse $ sort snakePosition
                    outOfBoundary = (mini^.x < 1) || (mini^.y < 1) ||
                                    (maxi^.x > numRectsX) || (maxi^.y > numRectsY)
                    selfCollision = (length (nub snakePosition)) /= (length snakePosition)

-- | Starting position & body
initialSnakePosition = [Point 6 6]
initialSnakePositionBottom = [Point 6 11]
initialSnake = Snake initialSnakePosition North 1
initialSnakeBottom = Snake initialSnakePositionBottom North 1

snakeEatsApple :: Snake -> Point -> Bool
snakeEatsApple snake apple = apple `elem` (snake^.position)

-- | Don't allow to change to opposite direction immediately
tryChangeSnakeDirection :: Direction -> Direction -> Direction
tryChangeSnakeDirection direction snakeDirection
            | oppositeDirections direction snakeDirection = snakeDirection
            | otherwise    = direction

changeSnakeDirection :: Snake -> Direction -> Snake
changeSnakeDirection snake dir = direction .~ (tryChangeSnakeDirection dir (snake^.direction)) $ snake
