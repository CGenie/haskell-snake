-- Board.hs -- board logic
{-# LANGUAGE TemplateHaskell #-}

module Board where

import Control.Lens

import Basic

data Board = Board {
     _xSize   :: Int,
     _ySize   :: Int
} deriving (Eq, Show)

makeLenses ''Board

initialBoard = Board numRectsX numRectsY
