module Main where

import Control.Lens

import Game.HSnake.AI
import Game.HSnake.Basic
import Game.HSnake.Game
import Game.HSnake.Graphics
import Game.HSnake.Player
import Game.HSnake.Snake

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.List
import Foreign.C.Types (CInt(..))
import qualified SDL
import qualified SDL.Font as SDLF


data MessageDir = MessageDir {
    quitMessage     :: Bool
}

data AppConfig = AppConfig {
    window      :: SDL.Window,
    renderer    :: SDL.Renderer,
    gameScreen  :: SDL.Surface,
    messageDir  :: MessageDir
}

type AppState = StateT GameState IO
type AppEnv = ReaderT AppConfig AppState


initEnv :: IO (AppConfig, GameState)
initEnv = do
  let windowSize = SDL.V2 (fromIntegral screenWidth :: CInt) (fromIntegral screenHeight :: CInt)
  let p = SDL.defaultWindow {SDL.windowInitialSize = windowSize}
  window <- SDL.createWindow "Haskell Snake" p
  SDL.showWindow window
  let screenSize = SDL.V2 (fromIntegral gameScreenWidth :: CInt) (fromIntegral gameScreenHeight :: CInt)
  gameScreen <- SDL.createRGBSurface screenSize SDL.RGB24
  -- Renderer renders to gameScreen, not to window
  -- We have text additionally to render to other parts of the window
  -- If we rendered directly to window and then rendered text, this
  -- would result in flickering
  renderer <- SDL.createSoftwareRenderer gameScreen

  let msgDir = MessageDir False

  applePos <- getRandomApple initialSnakePosition
  tick <- SDL.ticks

  return (AppConfig window renderer gameScreen msgDir,
            applePosition .~ applePos $
            lastTick .~ tick $ initialGameState) -- timerState


runLoop :: AppConfig -> GameState -> IO ()
runLoop = evalStateT . runReaderT loop


handleInputHuman :: GameState -> (Maybe SDL.Event) -> ReaderT AppConfig AppState ()
handleInputHuman gs (Just (SDL.Event _ (SDL.KeyboardEvent k))) =
  handleInputKeyboard gs k
handleInputHuman _ _ = return ()

handleInputKeyboard :: MonadState GameState m => GameState -> SDL.KeyboardEventData -> m ()
handleInputKeyboard gs (SDL.KeyboardEventData _ SDL.Pressed False keysym) =
  case SDL.keysymKeycode keysym of
      SDL.KeycodeDown  -> do
        put $ players .~ moveHuman South $ gs
      SDL.KeycodeUp    -> do
        put $ players .~ moveHuman North $ gs
      SDL.KeycodeLeft  -> do
        put $ players .~ moveHuman West $ gs
      SDL.KeycodeRight -> do
        put $ players .~ moveHuman East $ gs
      _          -> return ()
    where
      moveHuman dir = mapToIndices (\pl -> setNextPlayerDirection pl dir) (gs^.players) humanPlayerIndices
          where
              humanPlayerIndices = findIndices isHuman (gs^.players)
handleInputKeyboard _ _ = return ()



-- | poll for event until it is SDL_QUIT or NoEvent
whileEvents :: MonadIO m => (Maybe SDL.Event -> m ()) -> m Bool
whileEvents act = do
    event <- liftIO SDL.pollEvent
    case event of
        Just (SDL.Event _ SDL.QuitEvent) -> return True
        _                                -> do
            act event
            return False


loop :: AppEnv ()
loop = do
  gameState <- get
  quit <- whileEvents $ handleInputHuman gameState
  computeAINextMoves

  gameState <- get

  let apple = gameState^.applePosition
  let ps = gameState^.players

  if checkPlayersCollision ps
      then error "Game over"
      else return ()

  tick <- liftIO SDL.ticks

  let tickDifference = fromIntegral tick - fromIntegral (gameState^.lastTick)

  if (tickDifference > speedFromLevel (fromIntegral $ gameState^.level))
      then
          do
              -- Move the snake first, only then check for apple eating.
              -- Otherwise we would eat the apple, then immediately move
              -- the snake, which is wrong.
              updatePlayerDirections
              movePlayers
              gameState <- get
              put $ lastTick .~ tick $ gameState

              drawGame

              let appleEatersInd = findIndices (playerEatsApple apple) ps
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

  loop


computeAINextMoves :: ReaderT AppConfig AppState ()
computeAINextMoves = do
        gameState <- get
        put $ players .~ movedAI gameState $ gameState
    where
        aiIndices gs = findIndices (not . isHuman) (gs^.players)
        movedAI gs = mapToIndices (\pl -> setNextPlayerDirection pl (computeAIPlayerMove pl gs)) (gs^.players) (aiIndices gs)


updatePlayerDirections :: ReaderT AppConfig AppState ()
updatePlayerDirections = do
           gameState <- get
           put $ players .~ map updatePlayerDirection (gameState^.players) $ gameState


movePlayers :: ReaderT AppConfig AppState ()
movePlayers = do
            gameState <- get
            put $  players .~ map movePlayer (gameState^.players) $ gameState


increaseAppleEatersLength :: [Int] -> ReaderT AppConfig AppState ()
increaseAppleEatersLength appleEatersIndices = do
            gameState <- get
            put $ players .~ mapToIndices increasePlayerSnakeLength (gameState^.players) appleEatersIndices $ gameState


drawGame :: ReaderT AppConfig AppState ()
drawGame = do
  window      <- window `liftM` ask
  renderer    <- renderer `liftM` ask
  gameScreen  <- gameScreen `liftM` ask
  gameState   <- get

  liftIO $ do
    -- clear window surface first
    screen <- SDL.getWindowSurface window
    clearSurface screen

    paintBoard renderer
    paintApple renderer (gameState^.applePosition)
    mapM_ (paintPlayer renderer) (gameState^.players)

    SDL.present renderer

    showGameMessages window gameState

    _ <- SDL.surfaceBlit gameScreen Nothing screen Nothing

    SDL.updateWindowSurface window

    SDL.freeSurface screen

    return ()


main :: IO ()
main = do
  SDL.initializeAll
  SDLF.initialize

  (env, snakeState) <- initEnv

  runLoop env snakeState
