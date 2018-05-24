module TestSnake where

import Test.QuickCheck

-- to test rmDup
import qualified Data.Set as Set

import Basic
import Snake

-- tests for moving in various directions, with length 1
testSnakeMoveNorth (Point x y) =
                   moveSnake (SnakeState [Point x y] North 1) == (SnakeState [Point x (y+1)] North 1)
testSnakeMoveSouth (Point x y) =
                   moveSnake (SnakeState [Point x y] South 1) == (SnakeState [Point x (y-1)] South 1)
testSnakeMoveEast (Point x y) =
                   moveSnake (SnakeState [Point x y] East 1) == (SnakeState [Point (x+1) y] East 1)
testSnakeMoveWest (Point x y) =
                   moveSnake (SnakeState [Point x y] West 1) == (SnakeState [Point (x-1) y] West 1)

-- make sure that the turns are correct
testTurnSnakeNorth (Point x y) =
                   moveSnake (SnakeState [Point x y, Point (x+1) y] North 2) ==
                               (SnakeState [Point x (y+1), Point x y] North 2)
testTurnSnakeSouth (Point x y) =
                   moveSnake (SnakeState [Point x y, Point (x+1) y] South 2) ==
                               (SnakeState [Point x (y-1), Point x y] South 2)                   

-- duplicate removal tests
testrmDup1 = rmDup [1, 1, 2, 3, 1, 4, 3] == [1, 2, 3, 4]

testrmDup2 :: [Integer] -> Bool
testrmDup2 lst = (Set.size (Set.fromList lst)) == (length (rmDup lst))

instance Arbitrary Point where
         arbitrary = do
                   x <- arbitrary
                   y <- arbitrary
                   return (Point x y)

testsSnake = [
           ("testing snake move north", quickCheck testSnakeMoveNorth)
          ,("testing snake move south", quickCheck testSnakeMoveSouth)
          ,("testing snake move east", quickCheck testSnakeMoveEast)
          ,("testing snake move west", quickCheck testSnakeMoveWest)
          ,("testing snake turn north", quickCheck testTurnSnakeNorth)
          ,("testing snake turn south", quickCheck testTurnSnakeSouth)
          ,("testing rmDup1", quickCheck testrmDup1)
          ,("testing rmDup2 using Data.Set", quickCheck testrmDup2)]