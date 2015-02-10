{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Internal
import Data.HashMap.Lazy
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (isJust, fromJust)
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

findMatchingConnection :: WS.Connection -> ClientId -> ServerState -> Maybe WS.Connection
findMatchingConnection conn clientId (ServerState clients games waitingClient) =
  let matchingGames = HM.filter (\(Game cId1 cId2) -> cId1 == clientId || cId2 == clientId) games
      game = if HM.null matchingGames 
             then Nothing
             else Just ((snd . head . toList) matchingGames)
      otherClientId = liftM (\(Game cId1 cId2) -> if cId1 == clientId then cId2 else cId1) game
  in liftM (\cId -> clients ! cId) otherClientId

response :: WS.Connection -> ClientId -> MVar ServerState -> IO ()
response conn clientId state = forever $ do
  msg <- WS.receiveData conn
  -- WS.sendTextData conn ((action (decodeAction msg)) :: T.Text)

  -- if action is "StartGame", do the start game stuff that's currently in application
  -- otherwise, do the below

  currentState <- readMVar state
  let maybeOpponentConn = findMatchingConnection conn clientId currentState

  when (isJust maybeOpponentConn) $ WS.sendTextData (fromJust maybeOpponentConn) (msg :: T.Text)

application :: MVar ServerState -> WS.PendingConnection -> IO ()
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30    -- necessary to keep connection on some browsers

  ServerState clients games waitingClient <- liftIO $ readMVar state

  clientIdInt <- randomRIO (1, 100000000) :: IO Int
  let clientId = show clientIdInt

  if isJust waitingClient
  then
    liftIO $ modifyMVar_ state $ \s -> do 
      let s' = matchClients conn clientId s
      putStrLn $ show s'
      return s'
    -- send a GameStarted message to both clients with the state that was in the client message
  else
    liftIO $ modifyMVar_ state $ \s -> do 
      let s' = addWaitingClient conn clientId s
      putStrLn $ show s'
      return s'
  response conn clientId state

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" port $ application state
