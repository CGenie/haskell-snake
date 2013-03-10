{-# LANGUAGE FlexibleContexts #-}

module Main where

import Basic
import Board
import Game
import Snake

import Data.Word

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL as SDL

data MessageDir = MessageDir {
    quitMessage     :: Bool
}

data AppConfig = AppConfig {
    screen      :: Surface,
    gameScreen  :: Surface,
    messageDir  :: MessageDir
}

type AppState = StateT GameState IO
type AppEnv = ReaderT AppConfig AppState

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

getGameScreen :: MonadReader AppConfig m => m Surface
getGameScreen = liftM gameScreen ask

handleInput :: (MonadIO m, MonadState GameState m) => Event -> m ()
handleInput (KeyDown (Keysym key _ _))
                    | key == SDLK_a     = liftIO $ setCaption "asdf" []
                    | key == SDLK_q     = liftIO $ pushEvent Quit
                    | key == SDLK_DOWN  = changeDirection South
                    | key == SDLK_UP    = changeDirection North
                    | key == SDLK_LEFT  = changeDirection West
                    | key == SDLK_RIGHT = changeDirection East
                where
                    changeDirection direction = do
                        gameState <- get
                        snakeState <- snakeState `liftM` get
                        put gameState {snakeState = changeSnakeDirection snakeState direction }

handleInput _ = return ()

whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        Quit      -> return True
        NoEvent   -> return False
        _         -> do
            act event
            whileEvents act

main = withInit [InitEverything] $ do

    (env, snakeState) <- initEnv

    runLoop env snakeState


initEnv :: IO (AppConfig, GameState)
initEnv = do
    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    gameScreen <- createRGBSurface [SWSurface] gameScreenWidth gameScreenHeight screenBpp 0 0 0 0
    setCaption "Hello World" []

    colorWhite <- (mapRGB . surfaceGetPixelFormat) screen 0xff 0xff 0xff
    colorBlack <- (mapRGB . surfaceGetPixelFormat) screen 0x0f 0x0f 0x0f

    -- mapM_ is used to map a function over a list, this must return
    --   an IO monad sequence instead of a list, like the usual map does
    --   mapM_ is different from mapM in that it does not collect the results
    --   of mapped computation (see also sequence and sequence_ in the
    --   Control.Monad documentation)
    --mapM_ (\rect -> fillRect gameScreen rect bgColor) rects
    --paintRects gameScreen colorWhite
    paintBoard gameScreen

    --paintBlack <- paintRects gameScreen rects colorBlack

    let msgDir = MessageDir False 

    applePosition <- getRandomApple initialSnakePosition
    tick <- getTicks

    return (AppConfig screen gameScreen msgDir,
            initialGameState {applePosition = applePosition
                             ,lastSnakeMove = tick }) -- timerState)


runLoop :: AppConfig -> GameState -> IO ()
runLoop = evalStateT . runReaderT loop

loop :: AppEnv ()
loop = do
        quit <- whileEvents $ handleInput

        screen      <- screen `liftM` ask
        gameScreen  <- gameScreen `liftM` ask
        messageDir  <- messageDir `liftM` ask

        gameState <- get
        let ap = applePosition gameState
        let ss = snakeState gameState
        let sp = position ss

        drawGame

        if checkCollision sp
            then error "Game over"
            else return ()

        tick <- liftIO getTicks

        if (tick - (lastSnakeMove gameState) > 500 - 10*(fromIntegral $ level gameState)) then
            if snakeEatsApple sp ap
                then do
                    newApplePosition <- liftIO $ getRandomApple (position (newSnakeState ss))
                    -- start new level after apple is eaten
                    put initialGameState {
                            snakeState = newSnakeState ss
                           ,applePosition = newApplePosition
                           ,level = (level gameState) + 1}
                    
                else put (moveSnakeGameState gameState tick)
            else return ()

        unless quit loop
    where
        newSnakeState ss = initialSnakeState {
                                len = ((len ss) + 1)}
        moveSnakeGameState gameState tick =
                        gameState{ snakeState = (moveSnake (snakeState gameState))
                                  ,lastSnakeMove = tick }

drawGame = do
    screen      <- screen `liftM` ask
    gameScreen  <- gameScreen `liftM` ask
    gameState   <- get

    liftIO $ paintBoard gameScreen
    liftIO $ paintApple gameScreen (applePosition gameState)
    liftIO $ paintSnake gameScreen (snakeState gameState)

    liftIO $ blitSurface gameScreen Nothing screen Nothing

    liftIO $ SDL.flip screen
