{-# LANGUAGE ParallelListComp #-}
module Logic
  ( makeBotMoveIfNeed
  , getWinner
  , checkCellForBot
  ) where

import GameState
import Data.Foldable

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

checkCellForBot :: Int -> [Cell] -> (Int, Int) -> Int -> Bool
checkCellForBot n cells (x, y) 0 = if cells !! (x * n + y) == Empty 
                                   then True 
                                   else False 
checkCellForBot n cells (x, y) len = 
  case asum $ map checkWinner $   rowRight 
                               <> rowLeft 
                               <> colUp 
                               <> colDown 
                               <> diag1Up 
                               <> diag1Down
                               <> diag2Up
                               <> diag2Down of 
    Nothing -> False
    Just _ -> True
  where
    rowRight = if y + len < n 
               then [[cells !! (x * n + i) | i <- [y+1..y+len]]]
               else []
    rowLeft = if y - len >= 0
              then [[cells !! (x * n + i) | i <- [y-len..y-1]]]
              else []
    colUp = if x - len >= 0
            then [[cells !! (i * n + y) | i <- [x-len..x-1]]]
            else []
    colDown = if x + len < n 
              then [[cells !! (i * n + y) | i <- [x+1..x+len]]]
              else []
    diag1Up = if x + len < n && y + len < n
              then [[cells !! ((x + i) * n + y + i) | i <- [1..len]]]
              else []
    diag1Down = if x - len >= 0 && y - len >= 0
                then [[cells !! ((x - i) * n + y - i) | i <- [1..len]]]
                else []
    diag2Down = if x - len >= 0 && y + len < n
                then [[cells !! ((x - i) * n + y + i) | i <- [1..len]]]
                else []
    diag2Up = if x + len < n && y - len >= 0 
              then [[cells !! ((x + i) * n + y - i) | i <- [1..len]]]
              else []

findCellForBot :: Board -> (Int, Int) -> Int -> Maybe Int
findCellForBot board (x, y) len = 
  if y == boardSize board 
  then findCellForBot board (x+1, 0) len 
  else 
    if x == boardSize board
    then Nothing 
    else 
      let cells = boardCells board
          n     = boardSize  board in
        if cells !! (x * n + y) == Empty && checkCellForBot n cells (x, y) len 
        then Just $ x * boardSize board + y
        else findCellForBot board (x, y+1) len

makeBotMove :: GameState -> GameState
makeBotMove game = 
  let findCell = findCellForBot (gameBoard game) (0, 0) in 
    case asum $ [findCell 4, findCell 3, findCell 2, findCell 1, findCell 0] of 
      Nothing -> game 
      Just ind -> 
        let botPlayer = gamePlayer game 
            cells = boardCells $ gameBoard game
            newCells = insertToCells cells ind (Full botPlayer) in 
          checkIfGameOver $ changePlayer (changeBoardCells game newCells)

makeBotMoveIfNeed :: GameState -> GameState
makeBotMoveIfNeed game = 
  case gameState game of 
    GameOver _ -> game 
    Running -> case gameType game of 
      Humans -> game
      Comp a -> if a == gamePlayer game 
                then makeBotMove game
                else game
