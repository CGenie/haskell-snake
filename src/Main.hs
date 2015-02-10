{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

module Main where

import Control.Lens

import AI
import Basic
import Game
import Player
import Snake

import Data.List

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTFG

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

handleInputHuman :: GameState -> Event -> ReaderT AppConfig AppState ()
handleInputHuman gs (KeyboardEvent (SDL_KEYDOWN _ _ _ _ (Keysym _ key _))) =
                    case key of
                        SDLK_q     -> do
                                        liftIO $ pushEvent SDL_QUIT
                        SDLK_DOWN  -> do
                                   put $ players .~ moveHuman South $ gs
                        SDLK_UP    -> do
                                   put $ players .~ moveHuman North $ gs
                        SDLK_LEFT  -> do
                                   put $ players .~ moveHuman West $ gs
                        SDLK_RIGHT -> do
                                   put $ players .~ moveHuman East $ gs
                        _          -> return ()
                 where
                   moveHuman dir = mapToIndices (\pl -> setNextPlayerDirection pl dir) (gs^.players) humanPlayerIndices
                       where
                            humanPlayerIndices = findIndices isHuman (gs^.players)

handleInputHuman gs _ = return ()

-- | poll for event until it is SDL_QUIT or NoEvent
whileEvents :: MonadIO m => (Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO pollEvent
    case event of
        SDL_QUIT      -> return True
        --NoEvent   -> return False
        _         -> do
            act event
            whileEvents act

main = withInit [InitEverything] $ do

    (env, snakeState) <- initEnv

    runLoop env snakeState


initEnv :: IO (AppConfig, GameState)
initEnv = do
    result <- TTFG.init

    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    gameScreen <- createRGBSurface [SWSurface] gameScreenWidth gameScreenHeight screenBpp 0 0 0 0
    setCaption "Haskell Snake" []

    colorWhite <- (mapRGB . surfaceGetPixelFormat) screen 0xff 0xff 0xff
    colorBlack <- (mapRGB . surfaceGetPixelFormat) screen 0x0f 0x0f 0x0f

    paintBoard gameScreen

    let msgDir = MessageDir False 

    applePos <- getRandomApple initialSnakePosition
    tick <- getTicks

    return (AppConfig screen gameScreen msgDir,
            applePosition .~ applePos $
            lastTick .~ tick $ initialGameState) -- timerState

updatePlayerDirections :: ReaderT AppConfig AppState ()
updatePlayerDirections = do
           gameState <- get
           put $ players .~ map updatePlayerDirection (gameState^.players) $ gameState

computeAINextMoves :: ReaderT AppConfig AppState ()
computeAINextMoves = do
        gameState <- get
        put $ players .~ movedAI gameState $ gameState
    where        
        movedAI gs = mapToIndices (\pl -> setNextPlayerDirection pl (computeAIPlayerMove pl gs)) (gs^.players) (aiIndices gs)
            where
                aiIndices gs = findIndices (not . isHuman) (gs^.players)

movePlayers :: ReaderT AppConfig AppState ()
movePlayers = do
            gameState <- get
            put $  players .~ map movePlayer (gameState^.players) $ gameState

increaseAppleEatersLength :: [Int] -> ReaderT AppConfig AppState ()
increaseAppleEatersLength appleEatersIndices = do
            gameState <- get
            put $ players .~ mapToIndices increasePlayerSnakeLength (gameState^.players) appleEatersIndices $ gameState


runLoop :: AppConfig -> GameState -> IO ()
runLoop = evalStateT . runReaderT loop

loop :: AppEnv ()
loop = do
        gameState <- get
        quit <- whileEvents $ handleInputHuman gameState
        computeAINextMoves
        gameState <- get

        screen      <- screen `liftM` ask
        gameScreen  <- gameScreen `liftM` ask
        messageDir  <- messageDir `liftM` ask

        let ap = gameState^.applePosition
        let ps = gameState^.players

        drawGame

        if checkPlayersCollision ps
            -- TODO: increase level
            then error "Game over"
            else return ()

        tick <- liftIO getTicks

        let tickDifference = fromIntegral tick - fromIntegral (gameState^.lastTick)

        if (tickDifference > speedFromLevel (gameState^.level))
            then
                do
                    -- Move the snake first, only then check for apple eating.
                    -- Otherwise we would eat the apple, then immediately move
                    -- the snake, which is wrong.
                    updatePlayerDirections
                    movePlayers
                    gameState <- get
                    put $ lastTick .~ tick $ gameState

                    let appleEatersInd = findIndices (playerEatsApple ap) ps
                    if (length appleEatersInd > 0)
                        then do
                            newApplePosition <- liftIO $ getRandomApple (totalPlayersPosition ps)
                            -- increase snake's length
                            increaseAppleEatersLength appleEatersInd
                            gameState <- get
                            put $ applePosition .~ newApplePosition $ gameState
                            
                        else return ()
            else return ()

        if (shouldIncreaseLevel ps)
            then do
                newApplePosition <- liftIO $ getRandomApple (totalPlayersPosition ps)
                -- start new level after apple is eaten
                put $ applePosition .~ newApplePosition $
                      level .~ (gameState^.level) + 1 $ initialGameState
            else return ()

        unless quit loop

drawGame :: ReaderT AppConfig AppState ()
drawGame = do
    screen      <- screen `liftM` ask
    gameScreen  <- gameScreen `liftM` ask
    gameState   <- get

    liftIO $ do
        paintBoard gameScreen
        paintApple gameScreen (gameState^.applePosition)
        mapM_ (paintPlayer gameScreen) (gameState^.players)

        blitSurface gameScreen Nothing screen Nothing

        clearGameMessages screen
        showGameMessages screen gameState

        SDL.flip screen
