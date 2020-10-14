{-# LANGUAGE ParallelListComp #-}

module Logic
  ( changeState
  , initializeGame
  , getWinner
  ) where

import GameState
import Graphics.Gloss.Interface.IO.Game
import Render
import Data.Foldable
import Network.HTTP.Simple

isCellCoordCorrect :: Int -> (Int, Int) -> Bool 
isCellCoordCorrect n (x, y) = x >= 0 && x < n && y >= 0 && y < n

changeBoardCells :: GameState -> [Cell] -> GameState
changeBoardCells game newCell = 
    game {gameBoard = (gameBoard game) {boardCells = newCell}}

changePlayer :: GameState -> GameState 
changePlayer game = case gamePlayer game of 
  PlayerO -> game {gamePlayer = PlayerX}
  PlayerX -> game {gamePlayer = PlayerO}

insertToCells :: [Cell] -> Int -> Cell -> [Cell]
insertToCells cells ind cell = 
  let (st, en) = splitAt ind cells in 
  let (_, en') = (head en, tail en) in 
    st <> [cell] <> en'

applyPlayerMove :: GameState -> (Int, Int) -> GameState
applyPlayerMove game (x, y) = 
  let board = gameBoard game in
  let gameSize = boardSize $ board in 
  let player = gamePlayer game in
    if isCellCoordCorrect gameSize (x, y)
    then 
      let co = x * gameSize + y in
      let cells = boardCells $ board in
        if cells !! co == Empty
        then changePlayer $ changeBoardCells game (insertToCells cells co (Full player))
        else game
    else game

checkWinner :: [Cell] -> Maybe Player 
checkWinner (cell@(Full player):cells) = 
  if all (== cell) cells
  then Just player 
  else Nothing
checkWinner _ = Nothing

getWinner :: Board -> Maybe Player 
getWinner board = asum $ map checkWinner $ rows <> cols <> diags1 <> diags2'
  where 
    n = boardSize board
    cells = boardCells board
    winSize = min n 5
    rows  = concat [[[cells !! (i * n + j + q) | q <- [0..winSize-1]] 
                                               | i <- [0..n-1]] 
                                               | j <- [0..n-winSize]]
    cols  = concat [[[cells !! ((i + q) * n + j) | q <- [0..winSize-1]] 
                                                 | i <- [0..n-winSize]] 
                                                 | j <- [0..n-1]]
    diags1 = concat [[[cells !! ((i + q) * n + j + q) | q <- [0..winSize-1]] 
                                                      | i <- [0..n-winSize]]
                                                      | j <- [0..n-winSize]]
    diags2 = 
      concat [[[if j >= q && n-1-i+q < n then (n - 1 - i + q) * n + j - q else -1 
                                                      | q <- [0..winSize-1]]
                                                      | i <- [0..n-1]]
                                                      | j <- [0..n-1]]
    diags2' = map (\a -> map (cells !!) a) $ filter (\a -> not $ elem (-1 :: Int) a) diags2

countCells :: Cell -> [Cell] -> Int
countCells cell = length . filter ((==) cell)

checkIfGameOver :: GameState -> GameState
checkIfGameOver game = 
  case getWinner $ gameBoard game of 
    Nothing -> if countCells Empty (boardCells $ gameBoard game) == 0
               then game {gameState = GameOver Nothing}
               else game
    Just player -> game {gameState = GameOver $ Just player} 

playerMove :: GameState -> (Int, Int) -> GameState 
playerMove st coords = checkIfGameOver $ applyPlayerMove st coords 

mousePosToCellCoord :: Int -> (Float, Float) -> (Int, Int)
mousePosToCellCoord n (x, y) = ( floor ((y + fromIntegral screenHeight * 0.5) / (cellWeight n)) 
                               , floor ((x + fromIntegral screenWeight * 0.5) / (cellHeight n))
                               )

initializeGame :: Int -> IO GameState 
initializeGame n = do 
  request' <- parseRequest "GET http://127.0.0.1:8080/start"
  let fullRequest = setRequestBodyJSON n request'
  response <- httpJSON fullRequest :: IO (Response GameState)
  return $ getResponseBody response


makeServerMoveIfNeed :: GameState -> IO GameState
makeServerMoveIfNeed game = do 
  request' <- parseRequest "GET http://127.0.0.1:8080/game"
  let fullRequest = setRequestBodyJSON game request'
  response <- httpJSON fullRequest :: IO (Response GameState)
  return $ getResponseBody response

changeState :: Event -> GameState -> IO GameState
changeState (EventKey (MouseButton LeftButton) Up _ mousePos) game =
  let gameSize = boardSize $ gameBoard game in
    case gameState game of 
      Running -> do 
        let newSt = playerMove game (mousePosToCellCoord gameSize mousePos)
        newSt' <- makeServerMoveIfNeed newSt
        return newSt'
      GameOver _ -> makeServerMoveIfNeed $ initialState gameSize (gameType game)
changeState _ game = return game  
