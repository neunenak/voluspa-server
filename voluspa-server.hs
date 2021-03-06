{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (catch)
import Control.Applicative
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Internal as BSLI
import Data.HashMap.Lazy
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (isJust, fromJust, mapMaybe)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.Random

data ServerState = ServerState ClientsMap GamesMap WaitingClient deriving Show

type ClientsMap = HashMap ClientId Client
type GamesMap = HashMap ClientId ClientId   -- map of player to opponent (two entries per matchup). Maybe GamesMap is the wrong name for this?
type WaitingClient = Maybe ClientId

data Client = Client ClientName WS.Connection deriving Show

type ClientName = T.Text
type ClientId = Int

data Message =
  Message { action :: !T.Text, playerName :: !T.Text }
  deriving Show

instance FromJSON Message where
  parseJSON (Object v) =
    Message <$> v .: "action"
            <*> v .:? "playerName" .!= ""
  parseJSON _ = mzero

instance Show WS.Connection where
  show conn = "<Connection>"

newServerState :: ServerState
newServerState = ServerState HM.empty HM.empty Nothing

port :: Int
port = 22000

decodeMessage :: BSLI.ByteString -> Message
decodeMessage msg =
  let d = eitherDecode msg :: Either String Message
  in
    case d of
      Left err -> Message {action = "NoAction", playerName = ""}
      Right action -> action

addWaitingClient :: WS.Connection -> ClientId -> ClientName -> ServerState -> ServerState
addWaitingClient conn clientId clientName (ServerState clients games _) =
  -- First remove any matchups the client is currently in, then set the client as the waiting client
  let gamesWithClientRemoved = HM.filter (\oppId -> oppId /= clientId) $ HM.delete clientId games
  in
    ServerState (insert clientId (Client clientName conn) clients) gamesWithClientRemoved (Just clientId)

matchClients :: WS.Connection -> ClientId -> ClientName -> ServerState -> ServerState
matchClients conn clientId clientName (ServerState clients games (Just waitingClientId)) =
  -- First remove any matchups the client is currently in, then create a new matchup
  -- between the client and the waiting client
  let gamesWithClientRemoved = HM.filter (\oppId -> oppId /= clientId) $ HM.delete clientId games
      newGames = insert waitingClientId clientId $ insert clientId waitingClientId gamesWithClientRemoved
  in
    ServerState (insert clientId (Client clientName conn) clients) newGames Nothing

findMatchingClient :: ClientId -> ServerState -> Maybe Client
findMatchingClient clientId (ServerState clients games waitingClient) =
  let otherClientId = HM.lookup clientId games
  in liftM (\cId -> clients ! cId) otherClientId

removeClient :: ClientId -> ServerState -> ServerState
removeClient clientId (ServerState clients games waitingClient) =
  let clientsWithClientRemoved = HM.delete clientId clients
      gamesWithClientRemoved = HM.filter (\oppId -> oppId /= clientId) $ HM.delete clientId games
  in
    ServerState clientsWithClientRemoved gamesWithClientRemoved (mfilter (/= clientId) waitingClient)

response :: WS.Connection -> ClientId -> MVar ServerState -> IO ()
response conn clientId mvarState = forever $ do
  clientMsg <- WS.receiveData conn
  let clientMsgText = T.pack $ Char8.unpack $ BS.concat $ BSL.toChunks clientMsg
  let message@(Message action playerName) = decodeMessage clientMsg
  putStrLn (show (clientId, clientMsgText))

  state <- liftIO $ readMVar mvarState

  case action of
    "StartGame" -> do
      let ServerState _ _ waitingClient = state

      -- If the client is currently in a game, let the opponent know that this client disconnected
      -- TODO: this duplicates code from handleDisconnect - extract into a separate method?
      let maybeOpponentClient = findMatchingClient clientId state
      when (isJust maybeOpponentClient) $
        let Client _ opponentConn = (fromJust maybeOpponentClient)
        in WS.sendTextData opponentConn ("{\"action\":\"OpponentDisconnected\"}" :: T.Text) -- TODO: do this through Aeson

      case waitingClient of
        Just id | id == clientId -> return ()  -- If client is already waiting for a game, continue waiting

        Just opponentId -> liftIO $ modifyMVar_ mvarState $ \s -> do
          let newState@(ServerState clientMap _ _) = matchClients conn clientId playerName s
          let Client opponentName opponentConn = clientMap ! opponentId

          -- Assign the current player and opponent the colors Red and Blue, respectively,
          -- and pass on the StartGame messages.
          -- TODO: do this through Aeson rather than this ridiculous find-and-replace approach
          WS.sendTextData conn (T.replace "}" (T.concat [",\"color\":\"red\",\"opponentName\":\"", opponentName, "\"}"]) clientMsgText)
          WS.sendTextData opponentConn (T.replace "}" (T.concat [",\"color\":\"blue\",\"opponentName\":\"", playerName, "\"}"]) clientMsgText)

          putStrLn $ show newState
          return newState

        Nothing -> liftIO $ modifyMVar_ mvarState $ \s -> do
          let newState = addWaitingClient conn clientId playerName s
          putStrLn $ show newState
          return newState
    _ ->
      let maybeOpponentClient = findMatchingClient clientId state
      in when (isJust maybeOpponentClient) $
        let Client _ opponentConn = (fromJust maybeOpponentClient)
        in WS.sendTextData opponentConn clientMsgText


handleDisconnect :: WS.Connection -> ClientId -> MVar ServerState -> WS.ConnectionException -> IO ()
handleDisconnect conn clientId mvarState exception = do
  putStrLn $ "Client disconnected: " ++ show clientId

  state <- liftIO $ readMVar mvarState

  let maybeOpponentClient = findMatchingClient clientId state
  when (isJust maybeOpponentClient) $
    let Client _ opponentConn = (fromJust maybeOpponentClient)
    in WS.sendTextData opponentConn ("{\"action\":\"OpponentDisconnected\"}" :: T.Text) -- TODO: do this through Aeson

  liftIO $ modifyMVar_ mvarState $ \s -> do
    let newState = removeClient clientId s
    putStrLn $ show newState
    return newState

application :: MVar ServerState -> WS.PendingConnection -> IO ()
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30    -- necessary to keep connection on some browsers

  clientId <- randomRIO (1, 100000000) :: IO Int
  putStrLn $ "Client connected: " ++ show clientId
  catch (response conn clientId state) (handleDisconnect conn clientId state)

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" port $ application state
