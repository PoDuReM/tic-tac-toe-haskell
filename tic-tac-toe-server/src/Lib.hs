{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Lib
  ( startServer
  ) where

import Control.Monad.IO.Class
import GameState
import Logic
import Network.Wai.Handler.Warp
import Servant
import System.Random

type API = "game" :> ReqBody '[JSON] GameState :> Get '[JSON] GameState
         :<|> "start" :> ReqBody '[JSON] Int :> Get '[JSON] GameState

api :: Proxy API 
api = Proxy

app :: Application
app = serve api server

startServer :: IO ()
startServer = run 8080 app

randMode :: IO GType
randMode = do 
  a <- randomIO :: IO Bool 
  case a of 
    False -> return $ Comp PlayerX
    True -> return $ Comp PlayerO

makeMove :: GameState -> Handler GameState
makeMove game = do 
  liftIO $ putStrLn "bot turn"
  return $ makeBotMoveIfNeed game

initializeGame :: Int -> Handler GameState
initializeGame n = do
  liftIO $ putStrLn "new player connected!"
  rand <- liftIO $ randMode
  return $ makeBotMoveIfNeed $ initialState n rand

server :: Server API
server = makeMove :<|> initializeGame
