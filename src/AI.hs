module AI where

import Basic
import Game
import Player
import Snake

computeAIPlayerMove :: Player -> GameState -> Direction
computeAIPlayerMove pl gameState = circleAIPlayer pl


circleAIPlayer :: Player -> Direction
circleAIPlayer pl
    -- corner cases
    | x headPos == 1 && dir == West && y headPos <= 2                 = South
    | x headPos == 1 && dir == West && y headPos > 2                  = North
    | x headPos == (numRectsX - 1) && dir == East && y headPos <= 2   = South
    | x headPos == (numRectsX - 1) && dir == East && y headPos > 2    = North
    | y headPos == 1 && dir == North && x headPos >= 2                = West
    | y headPos == 1 && dir == North && x headPos < 2                 = East
    | y headPos == (numRectsY - 1) && dir == South && x headPos >= 2  = West
    | y headPos == (numRectsY - 1) && dir == South && x headPos < 2   = East
    -- normal operation
    | x headPos <= 2                                                  = South 
    | x headPos >= (numRectsX - 2)                                    = North
    | y headPos <= 2                                                  = West
    | y headPos >= (numRectsY - 2)                                    = East
    | otherwise                                                       = direction $ snake pl
    where
        headPos = head $ position $ snake pl
        dir     = direction $ snake pl