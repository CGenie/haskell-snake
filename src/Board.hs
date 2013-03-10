-- Board.hs -- board logic

module Board where

import Basic

data Board = Board {
     xSize   :: Int,
     ySize   :: Int
} deriving (Eq, Show)

initialBoard = Board numRectsX numRectsY
