{-# LANGUAGE FlexibleContexts #-}

module Main where

import AI
import Basic
import Board
import Game
import Player
import Snake

import Unsafe.Coerce

import Data.List
import Data.Word

import Control.Applicative
import Control.Concurrent (threadDelay)
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

data Hole = Hole
hole = undefined

getScreen :: MonadReader AppConfig m => m Surface
getScreen = liftM screen ask

getGameScreen :: MonadReader AppConfig m => m Surface
getGameScreen = liftM gameScreen ask

--handleInput :: (MonadIO m, MonadState GameState m) => Event -> m ()
--handleInput (KeyDown (Keysym key _ _)) =
--                    case key of
--                        SDLK_a     -> liftIO $ setCaption "asdf" []
--                        SDLK_q     -> liftIO $ pushEvent Quit
--                        SDLK_DOWN  -> changeNextSnakeDirection South
--                        SDLK_UP    -> changeNextSnakeDirection North
--                        SDLK_LEFT  -> changeNextSnakeDirection West
--                        SDLK_RIGHT -> changeNextSnakeDirection East
--                        _          -> return ()
--                where
--                    changeNextSnakeDirection dir = do
--                        gameState <- get
--                        snake <- snake `liftM` get
--                        put gameState {snakeState = changeSnakeDirection snakeState direction }
--                        put gameState {nextSnakeDirection = tryChangeSnakeDirection dir (direction snake)}

--handleInput _ = return ()

handleInputHuman :: GameState -> Event -> ReaderT AppConfig AppState ()
handleInputHuman gs (KeyDown (Keysym key _ _)) =
                    case key of
                        -- TODO: remove this, was just for testing
                        SDLK_a     -> do
                                        liftIO $ setCaption "asdf" []
                                        --return (direction $ snake pl)
                        SDLK_q     -> do
                                        liftIO $ pushEvent Quit
                                        --return (direction $ snake pl)
                        SDLK_DOWN  -> do
                                   --return () --return South
                                   put gs { players = moveHuman South }
                        SDLK_UP    -> do
                                   -- return () --return North
                                   put gs { players = moveHuman North }
                        SDLK_LEFT  -> do
                                   --return () --return West
                                   put gs { players = moveHuman West }
                        SDLK_RIGHT -> do
                                   --return () --return East
                                   put gs { players = moveHuman East }
                        _          -> return () --return (direction $ snake pl)
                 where
                   moveHuman dir = mapToIndices (\pl -> setNextPlayerDirection pl dir) (players gs) humanPlayerIndices
                       where
                            humanPlayerIndices = findIndices isHuman (players gs)

handleInputHuman gs _ = return () --return (direction $ snake pl)

-- | poll for event until it is Quit or NoEvent
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
    result <- TTFG.init

    screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
    gameScreen <- createRGBSurface [SWSurface] gameScreenWidth gameScreenHeight screenBpp 0 0 0 0
    setCaption "Haskell Snake" []

    colorWhite <- (mapRGB . surfaceGetPixelFormat) screen 0xff 0xff 0xff
    colorBlack <- (mapRGB . surfaceGetPixelFormat) screen 0x0f 0x0f 0x0f

    paintBoard gameScreen

    let msgDir = MessageDir False 

    applePosition <- getRandomApple initialSnakePosition
    tick <- getTicks

    return (AppConfig screen gameScreen msgDir,
            initialGameState {applePosition = applePosition
                             ,lastTick = tick}) -- timerState

updatePlayerDirections :: ReaderT AppConfig AppState ()
updatePlayerDirections = do
           gameState <- get
           put gameState { players = map updatePlayerDirection (players gameState) }

computeAINextMoves :: ReaderT AppConfig AppState ()
computeAINextMoves = do
        gameState <- get
        put gameState { players = movedAI gameState }
    where        
        movedAI gs = mapToIndices (\pl -> setNextPlayerDirection pl (computeAIPlayerMove pl gs)) (players gs) (aiIndices gs)
            where
                aiIndices gs = findIndices (not . isHuman) (players gs)

movePlayers :: ReaderT AppConfig AppState ()
movePlayers = do
            gameState <- get
            put gameState { players = map movePlayer (players gameState) }

increaseAppleEatersLength :: [Int] -> ReaderT AppConfig AppState ()
increaseAppleEatersLength appleEatersIndices = do
            gameState <- get
            put gameState { players = mapToIndices increasePlayerSnakeLength (players gameState) appleEatersIndices }


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

        let ap = applePosition gameState
        let ps = players gameState
        --let ss = snakeState gameState
        --let sp = position ss

        drawGame

        if (length (filter checkPlayerCollision ps) > 0)
            -- TODO: increase level
            then error "Game over"
            else return ()

        tick <- liftIO getTicks

        let tickDifference = fromIntegral tick - fromIntegral (lastTick gameState)

        if (tickDifference > speedFromLevel (level gameState))
            then
                do
                    -- Move the snake first, only then check for apple eating.
                    -- Otherwise we would eat the apple, then immediately move
                    -- the snake, which is wrong.
                    updatePlayerDirections
                    movePlayers
                    gameState <- get
                    put gameState { lastTick = tick }

                    let appleEatersInd = findIndices (playerEatsApple ap) ps
                    if (length appleEatersInd > 0)
                        then do
                            newApplePosition <- liftIO $ getRandomApple (totalPlayersPosition ps)
                            -- increase snake's length
                            increaseAppleEatersLength appleEatersInd
                            gameState <- get
                            put gameState { applePosition = newApplePosition }
                            
                        else return ()
            else return ()

        if (shouldIncreaseLevel ps)
            then do
                newApplePosition <- liftIO $ getRandomApple (totalPlayersPosition ps)
                -- start new level after apple is eaten
                put initialGameState {
                        players = [initialPlayer]
                       ,applePosition = newApplePosition
                       ,level = (level gameState) + 1}
            else return ()

        unless quit loop

drawGame = do
    screen      <- screen `liftM` ask
    gameScreen  <- gameScreen `liftM` ask
    gameState   <- get

    liftIO $ do
        paintBoard gameScreen
        paintApple gameScreen (applePosition gameState)
        mapM_ (paintPlayer gameScreen) (players gameState)

        blitSurface gameScreen Nothing screen Nothing

        clearGameMessages screen
        showGameMessages screen gameState

        SDL.flip screen
