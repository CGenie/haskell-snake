{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens

import Basic
import Board
import Player
import Snake

import Data.Word

import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF

screenWidth :: Int
screenWidth   = 640
screenHeight :: Int
screenHeight  = 480
screenBpp :: Int
screenBpp     = 32

gameScreenWidth :: Int
gameScreenWidth   = 540
gameScreenHeight :: Int
gameScreenHeight  = 400

messageScreenWidth :: Int
messageScreenWidth = screenWidth - gameScreenWidth
messageScreenHeight :: Int
messageScreenHeight = screenHeight - gameScreenHeight

rectWidth :: Int
rectWidth     = gameScreenWidth `div` numRectsX
rectHeight :: Int
rectHeight    = gameScreenHeight `div` numRectsY

data GameState = GameState {
     _players            :: [Player],
     _applePosition      :: Point,
     _board              :: Board,
     _level              :: Int,
     _lastTick           :: Word32
}
makeLenses ''GameState

initialGameState = GameState
                 [initialPlayer, initialComputer]
                 (Point 0 0)
                 initialBoard
                 1
                 0

rectFromPoint :: Point -> Maybe Rect
rectFromPoint (Point x y) = 
                Just (Rect ((x - 1)*rectWidth + 1)
                           ((y - 1)*rectHeight + 1)
                           (rectWidth - 1)
                           (rectHeight - 1))

rects = [rectFromPoint (Point x y) | x <- [1..numRectsX], y <- [1..numRectsY]]

{-|
  'mapM_' is used to map a function over a list, this must return
   an IO monad sequence instead of a list, like the usual map does.
   'mapM_' is different from 'mapM' in that it does not collect the results
   of mapped computation (see also 'sequence' and 'sequence_' in the
   "Control.Monad" documentation).
-}
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

paintSnake :: Surface -> Snake -> IO ()
paintSnake gameScreen snake =
                      mapM_ (paintSnakePiece gameScreen . rectFromPoint) (snake^.position)

paintPlayer :: Surface -> Player -> IO ()
paintPlayer gameScreen player = paintSnake gameScreen (player^.snake)

speedFromLevel level = toInteger $ floor $ (380 * (0.5**(0.1 * (fromIntegral level))) + 20)

-- if snake's length is greater than 10, then increase level
shouldIncreaseLevel :: [Player] -> Bool
shouldIncreaseLevel ps = length (filter (>=10) (map (\pl -> pl^.snake^.len) ps)) > 0


clearGameMessages screen = do
                    let messagesRect = Rect gameScreenWidth 0 messageScreenWidth messageScreenHeight
                    colorBlack <- (mapRGB . surfaceGetPixelFormat) screen 0x00 0x00 0x00
                    fillRect screen (Just messagesRect) colorBlack

--showGameMessages :: Surface -> IO ()
showGameMessages screen gameState = do
                    font <- openFont "liberation.ttf" 25

                    msgBlit levelMessage (Rect (surfaceGetWidth screen - 100) 10 100 100) font

                    --msgBlit lengthMessage (Rect (surfaceGetWidth screen - 100) 50 100 100) font

                where
                    levelMessage = "Level " ++ (show $ gameState^.level)
                    --lengthMessage = "Length " ++ (show $ len $ snake gameState)

                    renderMessage msg font = renderTextSolid font msg (Color 0xFF 0xFF 0xFF)
                    msgBlit msg rect font = do
                        rendered <- renderMessage msg font
                        blitSurface rendered Nothing screen (Just rect)
