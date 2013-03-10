module Game where

import Basic
import Board
import Snake

import Data.Word

import Control.Monad.State
import Control.Monad.Reader
import Graphics.UI.SDL as SDL

screenWidth :: Int
screenWidth   = 640
screenHeight :: Int
screenHeight  = 480
screenBpp :: Int
screenBpp     = 32
gameScreenWidth :: Int
--gameScreenWidth   = 540
gameScreenWidth = screenWidth
gameScreenHeight :: Int
--gameScreenHeight  = 400
gameScreenHeight = screenHeight

rectWidth :: Int
rectWidth     = gameScreenWidth `div` numRectsX
rectHeight :: Int
rectHeight    = gameScreenHeight `div` numRectsY

data GameState = GameState {
     snakeState         :: SnakeState,
     applePosition      :: Point,
     board              :: Board,
     level              :: Int,
     lastSnakeMove      :: Word32
}

initialGameState = GameState {snakeState = initialSnakeState
                             ,applePosition = Point 0 0
                             ,board = initialBoard
                             ,level = 1
                             ,lastSnakeMove = 0}

rectFromPoint :: Point -> Maybe Rect
rectFromPoint (Point x y) = 
                Just (Rect ((x - 1)*rectWidth + 1)
                           ((y - 1)*rectHeight + 1)
                           (rectWidth - 1)
                           (rectHeight - 1))

rects = [rectFromPoint (Point x y) | x <- [1..numRectsX], y <- [1..numRectsY]]

paintRects gameScreen color = liftIO $ mapM_ (\rect -> fillRect gameScreen rect color) rects

paintBoard :: Surface -> IO ()
paintBoard gameScreen = do
                       colorWhite <- (mapRGB . surfaceGetPixelFormat) gameScreen 0xff 0xff 0xff
                       paintRects gameScreen colorWhite
                       return ()

paintApple :: Surface -> Point -> IO ()
paintApple gameScreen applePosition = do
                       colorRed <- (mapRGB . surfaceGetPixelFormat) gameScreen 0xff 0x00 0x00
                       fillRect gameScreen (rectFromPoint applePosition) colorRed
                       return ()

paintSnakePiece :: Surface -> Maybe Rect -> IO ()
paintSnakePiece gameScreen rect = do
                           colorGreen <- (mapRGB . surfaceGetPixelFormat) gameScreen 0x00 0xff 0x00
                           fillRect gameScreen rect colorGreen
                           return ()

paintSnake :: Surface -> SnakeState -> IO ()
paintSnake gameScreen snakeState =
                      mapM_ (paintSnakePiece gameScreen . rectFromPoint) (position snakeState)
