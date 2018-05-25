-- Basic.hs -- basic functions and datatypes
module Game.HSnake.Basic where

import Control.Lens

import Data.List
import System.Random

--data Colors = White | Black

data Colour = White | Red | Blue | Green | Yellow | Black
  deriving (Eq, Show)

data Point = Point {
     _x          :: Int,
     _y          :: Int
} deriving (Show, Eq, Ord)

makeLenses ''Point

infixl 6 |+|
(|+|) :: Point -> Point -> Point
pt1 |+| pt2 = Point (pt1^.x + pt2^.x) (pt1^.y + pt2^.y)

data Direction = North | South | East | West
     deriving (Show, Eq, Ord)

-- | board size
numRectsX :: Int
numRectsX     = 11
-- | board size
numRectsY :: Int
numRectsY     = 11

oppositeDirections :: Direction -> Direction -> Bool
oppositeDirections dir1 dir2
            | ss == sort [North, South] = True
            | ss == sort [East, West]   = True
            | otherwise                 = False
        where
            ss = sort [dir1, dir2]

directionToPoint :: Direction -> Point
directionToPoint North = Point 0 (-1)
directionToPoint South = Point 0 1
directionToPoint East = Point 1 0
directionToPoint West = Point (-1) 0

randomListGenerator = do
            seed <- newStdGen
            return $ randomlist seed
        where
            randomlist = unfoldr (Just . random)

getRandomApple snakePosition = do
                r1 <- randomListGenerator
                r2 <- randomListGenerator
                let pts = map modPoint (zip r1 r2)
                return $ head $ filter (\pt -> (not . elem pt) snakePosition) pts
            where
                modPoint (x, y) = Point ((x `mod` numRectsX) + 1) ((y `mod` numRectsY) + 1)

-- | maps function to list, but only to specific indices
mapToIndices :: (Eq a) => (a -> a) -> [a] -> [Int] -> [a]
mapToIndices f xs ind = mapToIndices' f xs ind 0
        where
            mapToIndices' f' (x':xs') (ind':inds') i
                | ind' == i    = (f' x':mapToIndices' f' xs' inds' (i + 1))
                | otherwise    = (x':mapToIndices' f' xs' (ind':inds') (i + 1))
            mapToIndices' f' [] _ _ = []
            mapToIndices' f' xs' [] _ = xs'
