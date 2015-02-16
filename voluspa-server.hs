{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (catch)
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

data Message =
  Message { action :: !T.Text }
  deriving Show

instance FromJSON Message where
  parseJSON (Object v) =
    Message <$> v .: "action"
  parseJSON _ = mzero

instance Show WS.Connection where
  show conn = "<Connection>"

newServerState :: ServerState
newServerState = ServerState HM.empty HM.empty Nothing

port :: Int
port = 22000

decodeMessage :: ByteString -> Message
decodeMessage msg =
  let d = eitherDecode msg :: Either String Message
  in
    case d of
      Left err -> Message {action = "NoAction"}
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
response conn clientId mvarState = forever $ do
  clientMsg <- WS.receiveData conn
  state <- liftIO $ readMVar mvarState

  let message@(Message action) = decodeMessage clientMsg
  putStrLn $ show message

  case action of
    "StartGame" ->
      let ServerState _ _ waitingClient = state
      in case waitingClient of
        Just opponentId -> liftIO $ modifyMVar_ mvarState $ \s -> do
          let newState@(ServerState clientMap _ _) = matchClients conn clientId s
          let opponentConn = clientMap ! opponentId

          WS.sendTextData conn clientMsg
          WS.sendTextData opponentConn clientMsg

          putStrLn $ show newState
          return newState

        Nothing -> liftIO $ modifyMVar_ mvarState $ \s -> do
          let newState = addWaitingClient conn clientId s
          putStrLn $ show newState
          return newState
    _ ->
      let maybeOpponentConn = findMatchingConnection clientId state
      in when (isJust maybeOpponentConn) $ 
        WS.sendTextData (fromJust maybeOpponentConn) (clientMsg :: ByteString)


exceptionHandler :: WS.ConnectionException  -> IO ()
exceptionHandler e = putStrLn "Exception"

application :: MVar ServerState -> WS.PendingConnection -> IO ()
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30    -- necessary to keep connection on some browsers

  clientId <- randomRIO (1, 100000000) :: IO Int
  catch (response conn clientId state) exceptionHandler

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" port $ application state
