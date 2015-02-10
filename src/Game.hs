{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens

import Basic
import Board
import Player
import Snake

import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr

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
     _applePosition      :: Basic.Point,
     _board              :: Board,
     _level              :: Int,
     _lastTick           :: Word32
}
makeLenses ''GameState

initialGameState = GameState
                 [initialPlayer, initialComputer]
                 (Basic.Point 0 0)
                 initialBoard
                 1
                 0

rectFromPoint :: Basic.Point -> Rect
rectFromPoint (Basic.Point x y) = Rect (fromIntegral startX)
                                       (fromIntegral startY)
                                       (fromIntegral width)
                                       (fromIntegral height)
                where
                    (startX, startY, width, height) = (((x - 1)*rectWidth + 1), ((y - 1)*rectHeight + 1), (rectWidth - 1), (rectHeight - 1))

rects = [rectFromPoint (Basic.Point x y) | x <- [1..numRectsX], y <- [1..numRectsY]]

{-|
  'mapM_' is used to map a function over a list, this must return
   an IO monad sequence instead of a list, like the usual map does.
   'mapM_' is different from 'mapM' in that it does not collect the results
   of mapped computation (see also 'sequence' and 'sequence_' in the
   "Control.Monad" documentation).
-}
paintRects gameScreen color = do
    rectsPtr <- mapM new rects
    mapM_ (\rect -> fillRect gameScreen rect color) rectsPtr

paintBoard :: Surface -> IO ()
paintBoard gameScreen = do
                       colorWhite <- (mapRGB . surfaceFormat) gameScreen 0xff 0xff 0xff
                       gameScreenPtr <- new gameScreen
                       paintRects gameScreenPtr colorWhite
                       return ()

paintApple :: Surface -> Basic.Point -> IO ()
paintApple gameScreen applePosition = do
                       colorRed <- (mapRGB . surfaceFormat) gameScreen 0xff 0x00 0x00
                       gameScreenPtr <- new gameScreen
                       rectPtr <- new $ rectFromPoint applePosition
                       fillRect gameScreenPtr rectPtr colorRed
                       return ()

paintSnakePiece :: Surface -> Rect -> IO ()
paintSnakePiece gameScreen rect = do
                           colorGreen <- (mapRGB . surfaceFormat) gameScreen 0x00 0xff 0x00
                           gameScreenPtr <- new gameScreen
                           rectPtr <- new rect
                           fillRect gameScreenPtr rectPtr colorGreen
                           return ()

paintSnake :: Surface -> Snake -> IO ()
paintSnake gameScreen snake =
                      mapM_ (paintSnakePiece gameScreen . rectFromPoint) (snake^.position)

-- | TODO: paint different snakes with different colors
paintPlayer :: Surface -> Player -> IO ()
paintPlayer gameScreen player = paintSnake gameScreen (player^.snake)

speedFromLevel :: Integer -> Integer
speedFromLevel level = toInteger $ floor $ (380 * (0.5**(0.1 * (fromIntegral level))) + 20)

-- if snake's length is greater than 10, then increase level
shouldIncreaseLevel :: [Player] -> Bool
shouldIncreaseLevel ps = length (filter (>=10) (map (\pl -> pl^.snake^.len) ps)) > 0


clearGameMessages screen = do
                    let messagesRect = Rect (fromIntegral gameScreenWidth) (fromIntegral 0) (fromIntegral messageScreenWidth) (fromIntegral messageScreenHeight)
                    colorBlack <- (mapRGB . surfaceFormat) screen 0x00 0x00 0x00
                    screenPtr <- new screen
                    rectPtr <- new messagesRect
                    fillRect screenPtr rectPtr colorBlack

showGameMessages :: Surface -> GameState -> IO CInt
showGameMessages screen gameState = do
                    font <- openFont "liberation.ttf" 25

                    msgBlit levelMessage (Rect (surfaceW screen - 100) 10 100 100) font

                where
                    levelMessage = "Level " ++ (show $ gameState^.level)

                    renderMessage msg font = renderTextSolid font msg (Color 0xFF 0xFF 0xFF 0xFF)
                    msgBlit msg rect font = do
                        rendered <- renderMessage msg font
                        rectPtr <- new rect
                        screenPtr <- new screen
                        blitSurface rendered nullPtr screenPtr rectPtr
