module HSnake.Player where

import Control.Lens

import HSnake.Basic
import HSnake.Snake

import Data.List

data PlayerType = AI | Human
    deriving (Eq, Show)

data Player = Player {
    _playerType         :: PlayerType
   ,_snake              :: Snake
     {-|
          Temporary state variable to store next move's snake direction
          Without this when snake went North, and user quickly typed
          West, then South, snake would bump into itself
     -}
   ,_nextSnakeDirection :: Direction
} deriving (Eq, Show)

makeLenses ''Player

initialPlayer = Player 
              Human
              initialSnake
              (initialSnake^.direction)

initialPlayerBottom = Player
              Human
              initialSnakeBottom
              (initialSnakeBottom^.direction)

initialComputer = Player
              AI
              initialSnakeBottom
              (initialSnakeBottom^.direction)

isHuman :: Player -> Bool
isHuman (Player Human _ _) = True
isHuman _ = False 

checkPlayerCollision :: Player -> Bool
checkPlayerCollision player = checkCollision $ player^.snake

movePlayer :: Player -> Player
movePlayer pl = snake .~ (moveSnake $ pl^.snake) $ pl

playerEatsApple :: Point -> Player -> Bool
playerEatsApple apple player = snakeEatsApple (player^.snake) apple

sumPlayersPosition :: [Player] -> [Point]
sumPlayersPosition ps = foldl (++) [] $ map (\pl -> pl^.snake^.position) ps

totalPlayersPosition :: [Player] -> [Point]
totalPlayersPosition = nub . sumPlayersPosition

checkPlayersCollision :: [Player] -> Bool
checkPlayersCollision ps = (length (filter checkPlayerCollision ps) > 0) ||
                          (length (totalPlayersPosition ps) < (length (sumPlayersPosition ps)))

increasePlayerSnakeLength :: Player -> Player
increasePlayerSnakeLength pl = snake .~ (increaseSnakeLength $ pl^.snake) $ pl

setNextPlayerDirection :: Player -> Direction -> Player
setNextPlayerDirection pl dir = nextSnakeDirection .~ dir $ pl

updatePlayerDirection :: Player -> Player
updatePlayerDirection pl = snake .~ changeSnakeDirection (pl^.snake) (pl^.nextSnakeDirection) $ pl
