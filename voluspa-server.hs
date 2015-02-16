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
type GamesMap = HashMap ClientId ClientId   -- map of player to opponent (two entries per matchup). Maybe GamesMap is the wrong name for this?
type WaitingClient = Maybe ClientId

type ClientId = Int

data Action = 
  Action { action :: !T.Text }

instance Show WS.Connection where
  show conn = "<Connection>"

newServerState :: ServerState
newServerState = ServerState HM.empty HM.empty Nothing

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
  let newGames = (insert waitingClientId clientId (insert clientId waitingClientId games))
  in
    ServerState (insert clientId conn clients) newGames Nothing

findMatchingConnection :: ClientId -> ServerState -> Maybe WS.Connection
findMatchingConnection clientId (ServerState clients games waitingClient) =
  let otherClientId = HM.lookup clientId games
  in liftM (\cId -> clients ! cId) otherClientId

response :: WS.Connection -> ClientId -> MVar ServerState -> IO ()
response conn clientId state = forever $ do
  msg <- WS.receiveData conn
  ServerState clients games waitingClient <- liftIO $ readMVar state

  let Action action = decodeAction msg
  putStrLn $ show action

  when (action == "StartGame") $
    if isJust waitingClient
    then
      liftIO $ modifyMVar_ state $ \s -> do 
        let s' = matchClients conn clientId s

        let opponentConn = fromJust $ findMatchingConnection clientId s'
        WS.sendTextData conn ("{}" :: T.Text)
        WS.sendTextData opponentConn ("{}" :: T.Text)
        -- send a GameStarted message to both clients with the state that was in the client message
        
        putStrLn $ show s'
        return s'
    else
      liftIO $ modifyMVar_ state $ \s -> do 
        let s' = addWaitingClient conn clientId s
        putStrLn $ show s'
        return s'

  currentState <- readMVar state
  let maybeOpponentConn = findMatchingConnection clientId currentState

  when (isJust maybeOpponentConn) $ 
    WS.sendTextData (fromJust maybeOpponentConn) (msg :: ByteString)

application :: MVar ServerState -> WS.PendingConnection -> IO ()
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30    -- necessary to keep connection on some browsers

  clientId <- randomRIO (1, 100000000) :: IO Int

  response conn clientId state

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" port $ application state
