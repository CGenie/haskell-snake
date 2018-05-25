module Game.HSnake.AI where

import Control.Lens

import Game.HSnake.Basic
import Game.HSnake.Game
import Game.HSnake.Player
import Game.HSnake.Snake

computeAIPlayerMove :: Player -> GameState -> Direction
computeAIPlayerMove pl gameState = circleAIPlayer pl


circleAIPlayer :: Player -> Direction
circleAIPlayer pl
    -- corner cases
    | headPos^.x == 1 && dir == West && headPos^.y <= 2                 = South
    | headPos^.x == 1 && dir == West && headPos^.y > 2                  = North
    | headPos^.x == (numRectsX - 1) && dir == East && headPos^.y <= 2   = South
    | headPos^.x == (numRectsX - 1) && dir == East && headPos^.y > 2    = North
    | headPos^.y == 1 && dir == North && headPos^.x >= 2                = West
    | headPos^.y == 1 && dir == North && headPos^.x < 2                 = East
    | headPos^.y == (numRectsY - 1) && dir == South && headPos^.x >= 2  = West
    | headPos^.y == (numRectsY - 1) && dir == South && headPos^.x < 2   = East
    -- normal operation
    | headPos^.y <= 2                                                   = South
    | headPos^.y >= (numRectsX - 2)                                     = North
    | headPos^.y <= 2                                                   = West
    | headPos^.y >= (numRectsY - 2)                                     = East
    | otherwise                                                         = pl^.snake^.direction
    where
        headPos = head $ pl^.snake^.position
        dir     = pl^.snake^.direction
