module Main where

import Lib

main :: IO ()
main = do 
  putStrLn "insert the size of the field (from 3 to 20 recommended):"
  n <- readLn :: IO Int
  startGame n
