module HSnake.Graphics where

import HSnake.Basic
import HSnake.Board
import HSnake.Game
import HSnake.Player
import HSnake.Snake


import Control.Lens
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (pack)
import qualified Data.Vector.Storable as DVS
import Data.Word
import Linear
import Linear.Affine
import Foreign.C.Types (CInt(..))
import qualified SDL
import qualified SDL.Font as SDLF


type SDLRect = SDL.Rectangle CInt


setColor :: SDL.Renderer -> Colour -> IO ()
setColor r White  = SDL.rendererDrawColor r SDL.$= SDL.V4 maxBound maxBound maxBound maxBound
setColor r Red    = SDL.rendererDrawColor r SDL.$= SDL.V4 maxBound 0 0 maxBound
setColor r Green  = SDL.rendererDrawColor r SDL.$= SDL.V4 0 maxBound 0 maxBound
setColor r Blue   = SDL.rendererDrawColor r SDL.$= SDL.V4 0 0 maxBound maxBound
setColor r Yellow = SDL.rendererDrawColor r SDL.$= SDL.V4 maxBound maxBound 0 maxBound
setColor r Black = SDL.rendererDrawColor r SDL.$= SDL.V4 0 0 0 maxBound

sdlColor :: Colour -> SDLF.Color
sdlColor Black = SDL.V4 0 0 0 maxBound
sdlColor White = SDL.V4 maxBound maxBound maxBound maxBound

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
rectWidth  = gameScreenWidth `div` numRectsX
rectHeight :: Int
rectHeight = gameScreenHeight `div` numRectsY


rectFromPoint :: HSnake.Basic.Point -> SDLRect
rectFromPoint (HSnake.Basic.Point x y) = SDL.Rectangle startPoint size
                where
                    startX = (x - 1)*rectWidth + 1
                    startY = (y - 1)*rectHeight + 1
                    width  = rectWidth - 1
                    height = rectHeight - 1
                    startPoint = P $ V2 (fromIntegral startX) (fromIntegral startY)
                    size = V2 (fromIntegral width) (fromIntegral height)


rects = [rectFromPoint (HSnake.Basic.Point ptx pty) | ptx <- [1..numRectsX], pty <- [1..numRectsY]]
rectsVec = DVS.generate (numRectsX * numRectsY)
  (\i -> rects !! i )


paintRects :: SDL.Renderer -> Colour -> IO ()
paintRects r color = do
  setColor r color
  SDL.fillRects r rectsVec

paintBoard :: SDL.Renderer -> IO ()
paintBoard r = do
  paintRects r White

paintApple :: SDL.Renderer -> HSnake.Basic.Point -> IO ()
paintApple r ap = do
  setColor r Red
  SDL.fillRect r $ Just rect
  where
    rect = rectFromPoint ap

-- | TODO: paint different snakes with different colors
paintPlayer :: SDL.Renderer -> Player -> IO ()
paintPlayer r p = paintSnake r (p^.snake) (p^.colour)


paintSnake :: SDL.Renderer -> Snake -> Colour -> IO ()
paintSnake r s c =
  mapM_ (\p -> paintSnakePiece r (rectFromPoint p) c) (s^.position)

paintSnakePiece :: SDL.Renderer -> SDLRect -> Colour -> IO ()
paintSnakePiece r rect c = do
  setColor r c
  SDL.fillRect r $ Just rect

showGameMessages :: SDL.Window -> GameState -> IO ()
showGameMessages w gameState = do
  font <- SDLF.load "liberation.ttf" 25

  surface <- SDLF.solid font color levelMessage

  screen <- SDL.getWindowSurface w

  renderSurfaceToWindow w screen surface startPoint

  SDL.freeSurface screen
  SDL.freeSurface surface
  SDLF.free font

  where
    levelMessage = pack $ "Level " ++ (show $ gameState^.level)
    color = sdlColor White
    startPoint = P $ V2 (fromIntegral gameScreenWidth) 0

renderSurfaceToWindow :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Surface -> (SDL.Point V2 CInt) -> m ()
renderSurfaceToWindow w s i startPoint = do
  SDL.surfaceBlit i Nothing s (Just startPoint)
  return ()

clearScreen :: SDL.Renderer -> IO ()
clearScreen r = do
  setColor r Black
  SDL.clear r

clearSurface :: SDL.Surface -> IO ()
clearSurface s = do
  SDL.surfaceFillRect s Nothing (sdlColor Black)
