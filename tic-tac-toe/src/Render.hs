module Render
  ( stateToPicture
  , screenWeight
  , screenHeight
  , cellWeight
  , cellHeight
  ) where

import Graphics.Gloss
import GameState

screenWeight :: Int
screenWeight = 500

screenHeight :: Int 
screenHeight = 500

cellWeight :: Int -> Float
cellWeight n = fromIntegral screenWeight / fromIntegral n

cellHeight :: Int -> Float
cellHeight n = fromIntegral screenHeight / fromIntegral n

getColor :: Maybe Player -> Color 
getColor x = case x of 
  Just PlayerO -> blue
  Just PlayerX -> red
  Nothing      -> greyN 0.5

boardGridLines :: Int -> Int -> [Picture]
boardGridLines _ (-1) = []
boardGridLines n x = 
  let fx = fromIntegral x in 
  let line1 = line [ (fx * cellWeight n, 0.0)
                   , (fx * cellWeight n, fromIntegral screenHeight)
                   ] in 
  let line2 = line [ (0.0, fx * cellHeight n)
                   , (fromIntegral screenWeight, fx * cellHeight n)
                   ] in 
    line1 : line2 : boardGridLines n (x - 1)

boardGridPicture :: Int -> Picture
boardGridPicture n = pictures $ boardGridLines n n

xCellPicture :: Int -> Picture
xCellPicture n = pictures [rotate 45.0 line', rotate (-45.0) line'] 
  where
    line' = rectangleSolid side 5.0
    side = min (cellWeight n) (cellHeight n) * 0.75

oCellPicture :: Int -> Picture
oCellPicture n = thickCircle radius 5.0 
  where
    radius = min (cellWeight n) (cellHeight n) * 0.3

cellCoords :: Int -> Int -> (Float, Float)
cellCoords n ind = 
  let row    = ind `div` n in 
  let column = ind `mod` n in
        ( fromIntegral column * (cellWeight n) + (cellWeight n) * 0.5
        , fromIntegral row * (cellHeight n) + (cellHeight n) * 0.5 
        )  

cellPicture :: Int -> (Float, Float) -> Cell -> Picture
cellPicture n (x, y) cell = translate x y cellP
  where 
    cellP = case cell of 
              Full PlayerX -> xCellPicture n 
              Full PlayerO -> oCellPicture n 
              Empty        -> Blank

xCellsPictures :: Int -> Int -> [Cell] -> [Picture]
xCellsPictures _ _ [] = []
xCellsPictures n ind (x@(Full PlayerX):xs) =  cellPicture n (cellCoords n ind) x
                                            : xCellsPictures n (ind + 1) xs
xCellsPictures n ind (_:xs) = xCellsPictures n (ind + 1) xs

oCellsPictures :: Int -> Int -> [Cell] -> [Picture]
oCellsPictures _ _ [] = []
oCellsPictures n ind (x@(Full PlayerO):xs) =  cellPicture n (cellCoords n ind) x
                                            : oCellsPictures n (ind + 1) xs
oCellsPictures n ind (_:xs) = oCellsPictures n (ind + 1) xs


xCellsPicture :: Board -> Picture 
xCellsPicture board = pictures $ xCellsPictures (boardSize board) 0 (boardCells board)

oCellsPicture :: Board -> Picture 
oCellsPicture board = pictures $ oCellsPictures (boardSize board) 0 (boardCells board)

boardPicture :: Board -> Picture
boardPicture board = pictures $ [ boardGridPicture (boardSize board)
                                , xCellsPicture board
                                , oCellsPicture board
                                ]

gameOverPicture :: Maybe Player -> Board -> Picture 
gameOverPicture win board = color (getColor win) (boardPicture board)

runningPicture :: Board -> Picture 
runningPicture board = pictures [ color black $ boardGridPicture (boardSize board) 
                                , color red $ xCellsPicture board 
                                , color blue $ oCellsPicture board
                                ]

stateToPicture :: GameState -> IO Picture
stateToPicture game = return $ translate (fromIntegral screenWeight * (-0.5))
                                         (fromIntegral screenHeight * (-0.5))
                                         frame
  where frame = case gameState game of 
                  Running -> runningPicture (gameBoard game)
                  GameOver x -> gameOverPicture x (gameBoard game)
