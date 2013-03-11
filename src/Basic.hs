-- Basic.hs -- basic functions and datatypes

module Basic where

import Data.List
import System.Random

data Colors = White | Black

data Point = Point {
     x          :: Int,
     y          :: Int
} deriving (Show, Eq, Ord)

infixl 6 |+|
(Point x1 y1) |+| (Point x2 y2) = Point (x1 + x2) (y1 + y2)

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
