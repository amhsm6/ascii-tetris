{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens
import Data.Monoid
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import System.Random
import UI.NCurses

fieldWidth :: Int
fieldWidth = 20

fieldHeight :: Int
fieldHeight = 20

termWidth :: Int
termWidth = 120

termHeight :: Int
termHeight = 30

fieldBaseX :: Int
fieldBaseX = termWidth `div` 2 - fieldWidth `div` 2

fieldBaseY :: Int
fieldBaseY = fieldHeight

type Action = StateT GameState Curses

runAction :: Action a -> GameState -> Curses a
runAction = evalStateT

data GameState = GameState { __field :: V.Vector (V.Vector Bool)
                           , __figure :: Maybe Figure
                           , __image :: V.Vector (V.Vector Bool)
                           , __gravityTimeStamp :: POSIXTime
                           }

data Figure = Figure { __typ :: FigureType
                     , __rot :: Rotation
                     , __pos :: (Int, Int)
                     , __dat :: V.Vector (Int, Int)
                     }

data FigureType = O | I | S | Z | L | J | T
    deriving Enum

data Rotation = RotUp | RotRight | RotDown | RotLeft
    deriving Enum

makeLenses ''GameState
makeLenses ''Figure

data Input = MoveLeft | MoveRight | Rotate | ThrowDown

initialState :: GameState
initialState = GameState field Nothing image 0
    where field = V.replicate fieldHeight $ V.replicate fieldWidth False
          image = V.replicate termHeight $ V.replicate termWidth False

fieldBorderData :: [(Int, Int)]
fieldBorderData = bottom ++ lr
    where bottom = map (\x -> (x, fieldBaseY)) [fieldBaseX+1..fieldBaseX+fieldWidth]
          lr = [fieldBaseX, fieldBaseX+fieldWidth+1] >>= \x -> [0..fieldBaseY] >>= \y -> [(x, y)]

figureData :: FigureType -> Rotation -> V.Vector (Int, Int)

figureData O _ = V.fromList [(0, 0), (1, 0), (0, 1), (1, 1)]

figureData I RotUp = V.fromList [(0, -2), (0, -1), (0, 0), (0, 1)]
figureData I RotRight = V.fromList [(-2, 0), (-1, 0), (0, 0), (1, 0)]
figureData I RotDown = V.fromList [(0, -2), (0, -1), (0, 0), (0, 1)]
figureData I RotLeft = V.fromList [(-2, 0), (-1, 0), (0, 0), (1, 0)]

figureData S RotUp = V.fromList [(-1, -1), (0, -1), (0, 0), (1, 0)]
figureData S RotRight = V.fromList [(1, -1), (1, 0), (0, 0), (0, 1)]
figureData S RotDown = V.fromList [(-1, -1), (0, -1), (0, 0), (1, 0)]
figureData S RotLeft = V.fromList [(1, -1), (1, 0), (0, 0), (0, 1)]

figureData Z RotUp = V.fromList [(-1, 0), (0, 0), (0, -1), (1, -1)]
figureData Z RotRight = V.fromList [(-1, -1), (-1, 0), (0, 0), (0, 1)]
figureData Z RotDown = V.fromList [(-1, 0), (0, 0), (0, -1), (1, -1)]
figureData Z RotLeft = V.fromList [(-1, -1), (-1, 0), (0, 0), (0, 1)]

figureData L RotUp = V.fromList [(0, -1), (1, -1), (0, 0), (0, 1)]
figureData L RotRight = V.fromList [(-1, -1), (-1, 0), (0, 0), (1, 0)]
figureData L RotDown = V.fromList [(-1, 1), (0, 1), (0, 0), (0, -1)]
figureData L RotLeft = V.fromList [(-1, 0), (0, 0), (1, 0), (1, 1)]

figureData J RotUp = V.fromList [(-1, -1), (0, -1), (0, 0), (0, 1)]
figureData J RotRight = V.fromList [(-1, 1), (-1, 0), (0, 0), (1, 0)]
figureData J RotDown = V.fromList [(0, -1), (0, 0), (0, 1), (1, 1)]
figureData J RotLeft = V.fromList [(-1, 0), (0, 0), (1, 0), (1, -1)]

figureData T RotUp = V.fromList [(0, -1), (-1, 0), (0, 0), (1, 0)]
figureData T RotRight = V.fromList [(0, -1), (-1, 0), (0, 0), (0, 1)]
figureData T RotDown = V.fromList [(-1, 0), (0, 0), (1, 0), (0, 1)]
figureData T RotLeft = V.fromList [(0, -1), (0, 0), (1, 0), (0, 1)]

recalc :: Action ()
recalc = do
    zoom (_figure . _Just) $ do
        typ <- use _typ
        rot <- use _rot
        _dat .= figureData typ rot

        (dx, dy) <- use _pos
        _dat . traverse . _1 += dx
        _dat . traverse . _2 += dy

check :: Action Bool
check = do
    s <- get
    hit <- execWriterT $ forMOf_ (_figure . _Just . _dat . traverse) s $ \(x, y) -> do
        hit <- use $ pre (_field . ix y . ix x) . non False
        tell $ Any $ hit || x < 0 || x >= fieldWidth || y < 0

    pure $ hit ^. _Wrapped

input :: Action (Maybe Input)
input = do
    ev <- lift $ do
        w <- defaultWindow
        getEvent w $ Just 0

    case ev of
        Nothing -> pure Nothing
        Just (EventSpecialKey KeyLeftArrow) -> pure $ Just MoveLeft
        Just (EventSpecialKey KeyRightArrow) -> pure $ Just MoveRight
        Just (EventSpecialKey KeyUpArrow) -> pure $ Just Rotate
        Just (EventCharacter ' ') -> pure $ Just ThrowDown
        _ -> pure Nothing

process :: Input -> Action ()

process MoveLeft = do
    _figure . _Just . _pos . _1 -= 1
    _figure . _Just . _dat . traverse . _1 -= 1

    hit <- check
    when hit $ do
        _figure . _Just . _pos . _1 += 1
        _figure . _Just . _dat . traverse . _1 += 1

process MoveRight = do
    _figure . _Just . _pos . _1 += 1
    _figure . _Just . _dat . traverse . _1 += 1

    hit <- check
    when hit $ do
        _figure . _Just . _pos . _1 -= 1
        _figure . _Just . _dat . traverse . _1 -= 1

process Rotate = do
    fig <- use _figure

    _figure . _Just . _rot %= toEnum . (`mod`4) . (+1) . fromEnum
    recalc

    hit <- check
    when hit $ _figure .= fig

process ThrowDown = do
    let drop = do
            hit <- check
            unless hit $ do
                _figure . _Just . _dat . traverse . _2 -= 1
                drop
    drop

    s <- get
    forMOf_ (_figure . _Just . _dat . traverse) s $ \(x, y) -> do
        _field . ix (y + 1) . ix x .= True

    _figure .= Nothing

spawn :: Action ()
spawn = do
    typ <- liftIO $ toEnum <$> randomRIO (0, 6)
    rot <- liftIO $ toEnum <$> randomRIO (0, 3)

    _figure .= Just (Figure typ rot (fieldWidth `div` 2, fieldHeight) V.empty)
    recalc

update :: Action ()
update = do
    s <- get
    when (has (_figure . _Nothing) s) spawn

    prev <- use _gravityTimeStamp
    curr <- liftIO getPOSIXTime
    when (curr - prev > 0.2) $ do
        _gravityTimeStamp .= curr

        _figure . _Just . _pos . _2 -= 1
        _figure . _Just . _dat . traverse . _2 -= 1

    hit <- check
    when hit $ do
        s <- get
        forMOf_ (_figure . _Just . _dat . traverse) s $ \(x, y) -> do
            _field . ix (y + 1) . ix x .= True

        _figure .= Nothing

    s <- get
    forMOf_ (backwards $ _field . itraversed . filtered V.and . asIndex) s $ \i -> do
        forM_ [i..fieldHeight-1] $ \j -> do
            row <- use $ pre (_field . ix (j + 1)) . non (V.replicate fieldWidth False)
            _field . ix j .= row

draw :: Action ()
draw = do
    _image .= initialState ^. _image

    forM_ fieldBorderData $ \(x, y) -> do
        _image . ix y . ix x .= True

    s <- get
    forMOf_ (_figure . _Just . _dat . traverse) s $ \(x, y) -> do
        let x' = fieldBaseX + 1 + x
            y' = fieldBaseY - 1 - y
        _image . ix y' . ix x' .= True

    s <- get
    iforMOf_ (_field . itraversed <.> itraversed) s $ \(y, x) c -> do
        let x' = fieldBaseX + 1 + x
            y' = fieldBaseY - 1 - y
        _image . ix y' . ix x' ||= c

    s <- get
    lift $ do
        w <- defaultWindow
        updateWindow w $ do
            moveCursor 0 0
            iforMOf_ (_image . itraversed <.> itraversed) s $ \(y, x) c -> do
                unless (x == 119 && y == 29) $ if c then drawString "*" else drawString " "
        render

game :: Action ()
game = forever $ do
    input >>= maybe (pure ()) process
    update
    draw

main :: IO ()
main = runCurses $ runAction game initialState
