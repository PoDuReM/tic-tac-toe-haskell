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

checkCellForBotTest :: SpecWith()
checkCellForBotTest = describe "checkCellForBot test" $ do 
  it "check with cellsNobody" $ do 
    checkCellForBot 3 cellsNobody (0, 0) 1 `shouldBe` True
    checkCellForBot 3 cellsNobody (0, 0) 2 `shouldBe` False
    checkCellForBot 3 cellsNobody (1, 0) 1 `shouldBe` True
    checkCellForBot 3 cellsNobody (1, 0) 2 `shouldBe` False
  it "check with cellsX" $ do 
    checkCellForBot 3 cellsX (0, 2) 1 `shouldBe` True
    checkCellForBot 3 cellsX (0, 2) 2 `shouldBe` False
    checkCellForBot 3 cellsX (2, 1) 1 `shouldBe` True
    checkCellForBot 3 cellsX (2, 1) 2 `shouldBe` False
  it "check with cellsO" $ do
    checkCellForBot 3 cellsO (1, 1) 1 `shouldBe` True
    checkCellForBot 3 cellsO (1, 1) 2 `shouldBe` False
    checkCellForBot 3 cellsO (1, 1) 0 `shouldBe` True
    checkCellForBot 3 cellsO (0, 1) 0 `shouldBe` False

main :: IO ()
main = hspec $ do
  checkWinnerTest
  checkCellForBotTest