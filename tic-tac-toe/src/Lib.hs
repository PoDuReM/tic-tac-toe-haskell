module Lib
  ( startGame
  ) where

import Graphics.Gloss
import Render
import Logic
import Graphics.Gloss.Interface.IO.Game

window :: Display
window = InWindow "tic-tac-toe" (screenHeight, screenWeight) (100, 100)

startGame :: Int -> IO ()
startGame n = do
  initState <- initializeGame n
  playIO window white (30 :: Int) initState stateToPicture changeState (\_ -> return . id)
