{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Internal
import Data.HashMap.Lazy
import qualified Data.Text as T
import qualified Network.WebSockets as WS

data ServerState = ServerState ClientsMap GamesMap WaitingClient

type ClientsMap = HashMap ClientId WS.Connection
type GamesMap = HashMap GameId Game
type WaitingClient = Maybe ClientId

data Game = Game ClientId ClientId

type ClientId = String
type GameId = String

data Action = 
  Action { action :: !T.Text } deriving Show

newServerState :: ServerState
newServerState = ServerState Data.HashMap.Lazy.empty Data.HashMap.Lazy.empty Nothing

instance FromJSON Action where
  parseJSON (Object v) =
    Action <$> v .: "action"
  parseJSON _ = mzero

echo :: WS.Connection -> IO ()
echo conn = forever $ do
  msg <- WS.receiveData conn
  -- WS.sendTextData conn ((action (decodeAction msg)) :: T.Text)
  WS.sendTextData conn (msg :: T.Text)

decodeAction :: ByteString -> Action
decodeAction msg = 
  let d = eitherDecode msg :: Either String Action
  in
    case d of
      Left err -> Action {action = "NoAction"}
      Right action -> action

port :: Int
port = 22000

application :: MVar ServerState -> WS.PendingConnection -> IO ()
application state pend = do
  conn <- WS.acceptRequest pend
  echo conn

main :: IO ()
main = do
  state <- newMVar newServerState
  WS.runServer "0.0.0.0" port $ application state
