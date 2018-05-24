module Main where

import Test.QuickCheck
import Text.Printf

import TestSnake

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) testsSnake
