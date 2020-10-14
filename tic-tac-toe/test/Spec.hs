module Main
  ( main
  ) where

import GameState
import Logic
import Test.Hspec (hspec)
import Test.Hspec (SpecWith, describe, shouldBe, it)

cellsNobody :: [Cell]
cellsNobody = [ Empty       , Empty       , Empty
              , Empty       , Full PlayerX, Empty
              , Full PlayerO, Empty       , Empty
              ]

boardNobody :: Board 
boardNobody = Board 3 cellsNobody

cellsX :: [Cell]
cellsX = [ Full PlayerX, Full PlayerO, Empty 
         , Empty       , Full PlayerX, Full PlayerO
         , Full PlayerO, Empty       , Full PlayerX 
         ]

boardX :: Board 
boardX = Board 3 cellsX

cellsO :: [Cell]
cellsO = [ Empty       , Full PlayerX, Full PlayerO 
         , Full PlayerX, Empty       , Full PlayerO
         , Empty       , Full PlayerX, Full PlayerO 
         ]

boardO :: Board 
boardO = Board 3 cellsO

checkWinnerTest :: SpecWith ()
checkWinnerTest = describe "checkWinner function test" $ do 
  it "check nobody wins" $ 
    getWinner boardNobody `shouldBe` Nothing
  it "check x wins" $ 
    getWinner boardX `shouldBe` Just PlayerX
  it "check o wins" $
    getWinner boardO `shouldBe` Just PlayerO

main :: IO ()
main = hspec $ do
  checkWinnerTest