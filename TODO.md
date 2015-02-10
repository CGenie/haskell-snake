SDL2 migration
==============

Some sample code
----------------
(taken from http://d.hatena.ne.jp/satosystems/20141220/1419083502 )

```
module Main where

import qualified Graphics.UI.SDL as SDL
import Control.Monad
import Data.Either
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

main :: IO ()
main = do
  sdlInit SDL.SDL_INIT_EVERYTHING >>= either sdlError return
  window <- sdlCreateWindow "hello" 640 480 >>= either sdlError return
  path <- newCString "hello.bmp"
  image <- SDL.loadBMP path
  free path
  drawImage window image 320 240
  waitEvent window image
  SDL.destroyWindow window
  SDL.quit

sdlInit :: SDL.InitFlag -> IO (Either String ())
sdlInit flag = do
  rc <- SDL.init flag
  return $ if rc == 0
    then Right()
    else Left $ "SDL.init error:" ++ (show $ fromEnum rc)

sdlCreateWindow :: String -> CInt -> CInt -> IO (Either String SDL.Window)
sdlCreateWindow windowTitle width height =
  withCAString windowTitle $ \title -> do
    window <- SDL.createWindow
      title
      SDL.SDL_WINDOWPOS_UNDEFINED
      SDL.SDL_WINDOWPOS_UNDEFINED
      width
      height SDL.SDL_WINDOW_SHOWN
    return $ if window /= nullPtr
      then Right window
      else Left "SDL.createWindow returns null pointer"

sdlWaitEvent :: IO (CInt, SDL.Event)
sdlWaitEvent = alloca $ \ptr -> do
  rc <- SDL.waitEvent ptr
  event <- peek ptr
  return (rc, event)

waitEvent :: SDL.Window -> Ptr SDL.Surface -> IO ()
waitEvent window image = do
  (rc, event) <- sdlWaitEvent
  if rc == 1
    then case event of
      (SDL.QuitEvent _ _)                      -> return ()
      (SDL.MouseMotionEvent _ _ _ _ 1 x y _ _) -> drawImage window image (fromIntegral x) (fromIntegral y) >> waitEvent window image
      (SDL.MouseButtonEvent _ _ _ _ _ 1 _ x y) -> drawImage window image (fromIntegral x) (fromIntegral y) >> waitEvent window image
      _                                        -> waitEvent window image
    else print $ "SDL.waitEvent error:" ++ (show $ fromEnum rc)

sdlError :: String -> IO a
sdlError message = do
  errMsg <- SDL.getError >>= peekCString
  fail (message ++ " (" ++ errMsg ++ ")")

drawImage :: SDL.Window -> Ptr SDL.Surface -> Int -> Int -> IO ()
drawImage window image x y = do
  surface <- SDL.getWindowSurface window
  width <- liftM SDL.surfaceW $ peek image
  height <- liftM SDL.surfaceH $ peek image
  format <- liftM SDL.surfaceFormat $ peek surface
  color <- SDL.mapRGB format 0xFF 0xFF 0xFF
  SDL.fillRect surface nullPtr color
  rect <- new $ SDL.Rect
   (fromIntegral (getCenter x width))
   (fromIntegral (getCenter y height))
   width
   height
  SDL.blitSurface image nullPtr surface rect
  free rect
  SDL.updateWindowSurface window
  return ()
  where
    getCenter xy wh = xy - ((fromIntegral wh) `div` 2)
```
