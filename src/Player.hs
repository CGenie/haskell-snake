module Player where

import Basic
import Snake

import Data.List

data PlayerType = AI | Human
    deriving (Eq, Show)

data Player = Player {
    playerType         :: PlayerType
   ,snake              :: Snake
     -- | Temporary state variable to store next move's snake direction
     --   Without this when snake went North, and user quickly typed
     --   West, then South, snake would bump into itself
   ,nextSnakeDirection :: Direction
} deriving (Eq, Show)

initialPlayer = Player {
    playerType         = Human
   ,snake              = initialSnake
   ,nextSnakeDirection = direction initialSnake
}

initialPlayerBottom = Player {
    playerType         = Human
   ,snake              = initialSnakeBottom
   ,nextSnakeDirection = direction initialSnakeBottom
}

initialComputer = Player {
    playerType         = AI
   ,snake              = initialSnakeBottom
   ,nextSnakeDirection = direction initialSnakeBottom
}

isHuman :: Player -> Bool
isHuman (Player Human _ _) = True
isHuman _ = False 

checkPlayerCollision :: Player -> Bool
checkPlayerCollision player = checkCollision $ snake player

movePlayer :: Player -> Player
movePlayer  pl = pl { snake = (moveSnake $ snake pl) }

playerEatsApple :: Point -> Player -> Bool
playerEatsApple apple player = snakeEatsApple (snake player) apple

totalPlayersPosition :: [Player] -> [Point]
totalPlayersPosition ps = nub $ foldl (++) [] $ map (position . snake) ps

increasePlayerSnakeLength :: Player -> Player
increasePlayerSnakeLength pl = pl { snake = increaseSnakeLength $ snake pl }

setNextPlayerDirection :: Player -> Direction -> Player
setNextPlayerDirection pl dir = pl { nextSnakeDirection = dir }

updatePlayerDirection :: Player -> Player
updatePlayerDirection pl = pl { snake = changeSnakeDirection (snake pl) (nextSnakeDirection pl) }
