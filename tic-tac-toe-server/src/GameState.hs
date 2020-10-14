{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GameState
  ( initialState
  , Player (..)
  , Cell (..)
  , Board (..)
  , GType (..)
  , State (..)
  , GameState (..)
  ) where

import GHC.Generics ( Generic )
import Data.Aeson

data Player = PlayerX | PlayerO deriving (Generic, Show, Eq)

instance ToJSON Player where 
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Player

data Cell = Empty | Full Player deriving (Generic, Show, Eq)

instance ToJSON Cell where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Cell

data Board = Board 
  { boardSize :: Int
  , boardCells :: [Cell]
  } deriving (Generic, Show, Eq)   

instance ToJSON Board where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Board

data GType = Humans | Comp Player deriving (Generic, Show, Eq)

instance ToJSON GType where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON GType

data State = Running | GameOver (Maybe Player) deriving (Generic, Show, Eq)

instance ToJSON State where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON State

data GameState = GameState 
  { gameBoard :: Board
  , gamePlayer :: Player
  , gameType :: GType
  , gameState :: State
  } deriving (Generic, Eq, Show)

instance ToJSON GameState where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON GameState

initialState :: Int -> GType -> GameState
initialState n gtype = GameState { gameBoard = Board n $ replicate (n*n) Empty
                                 , gamePlayer = PlayerX
                                 , gameType = gtype
                                 , gameState = Running }
