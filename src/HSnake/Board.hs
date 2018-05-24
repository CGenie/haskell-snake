-- Board.hs -- board logic
module HSnake.Board where

import Control.Lens

import HSnake.Basic

data Board = Board {
     _xSize   :: Int,
     _ySize   :: Int
} deriving (Eq, Show)

makeLenses ''Board

initialBoard = Board numRectsX numRectsY
