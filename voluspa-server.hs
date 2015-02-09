{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Internal
import Data.HashMap.Lazy
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.Random

data ServerState = ServerState ClientsMap GamesMap WaitingClient deriving Show

type ClientsMap = HashMap ClientId WS.Connection
type GamesMap = HashMap GameId Game
type WaitingClient = Maybe ClientId

data Game = Game ClientId ClientId deriving Show

type ClientId = String
type GameId = String

data Action = 
  Action { action :: !T.Text }

instance Show WS.Connection where
  show conn = "<Connection>"

newServerState :: ServerState
newServerState = ServerState Data.HashMap.Lazy.empty Data.HashMap.Lazy.empty Nothing

port :: Int
port = 22000

instance FromJSON Action where
  parseJSON (Object v) =
    Action <$> v .: "action"
  parseJSON _ = mzero

decodeAction :: ByteString -> Action
decodeAction msg = 
  let d = eitherDecode msg :: Either String Action
  in
    case d of
      Left err -> Action {action = "NoAction"}
      Right action -> action

addWaitingClient :: WS.Connection -> ClientId -> ServerState -> ServerState
addWaitingClient conn clientId (ServerState clients games waitingClient) = 
  ServerState (insert clientId conn clients) games (Just clientId)

matchClients :: WS.Connection -> ClientId -> ServerState -> ServerState
matchClients conn clientId (ServerState clients games (Just waitingClientId)) = 
  let gameId = clientId ++ waitingClientId
  in
    ServerState (insert clientId conn clients) (insert gameId (Game clientId waitingClientId) games) Nothing

response :: WS.Connection -> MVar ServerState -> IO ()
response conn state = forever $ do
  msg <- WS.receiveData conn
  -- WS.sendTextData conn ((action (decodeAction msg)) :: T.Text)
  WS.sendTextData conn (msg :: T.Text)

application :: MVar ServerState -> WS.PendingConnection -> IO ()
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30    -- necessary to keep connection on some browsers

  ServerState clients games waitingClient <- liftIO $ readMVar state

  clientId <- randomRIO (1, 100000000) :: IO Int    -- TODO: should be a random string, so we don't have to (show clientId) everywhere

  if isJust waitingClient
  then
    liftIO $ modifyMVar_ state $ \s -> do 
      let s' = matchClients conn (show clientId) s
      putStrLn $ show s'
      return s'
  else
    liftIO $ modifyMVar_ state $ \s -> do 
      let s' = addWaitingClient conn (show clientId) s
      putStrLn $ show s'
      return s'
  response conn state

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" port $ application state
