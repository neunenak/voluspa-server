{-# LANGUAGE OverloadedStrings #-}

import qualified Network.WebSockets as WS
import qualified Data.Text as T
import Control.Monad (forever)


echo :: WS.Connection -> IO ()
echo conn = forever $ do
    msg <- WS.receiveData conn
    WS.sendTextData conn (T.append msg " meow")

port :: Int
port = 22000

application :: WS.PendingConnection -> IO ()
application pend = do
    conn <- WS.acceptRequest pend
    echo conn

main = WS.runServer "0.0.0.0" port application
