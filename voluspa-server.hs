{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Internal
import qualified Data.Text as T
import qualified Network.WebSockets as WS

data Action = 
  Action { action :: !T.Text } deriving Show

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

application :: WS.PendingConnection -> IO ()
application pend = do
    conn <- WS.acceptRequest pend
    echo conn

main = WS.runServer "0.0.0.0" port application
