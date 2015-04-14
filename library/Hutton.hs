{-# LANGUAGE OverloadedStrings #-}

module Hutton where

import Hutton.JSON (Message)
import Hutton.WebSockets (runSecureClient)

import Control.Monad (forever, void)
import Data.Aeson (eitherDecode)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy.Char8 (unpack)
import Flow
import Network.HTTP.Types (encodePath)
import Network.Socket (PortNumber)
import Network.WebSockets (ClientApp, receiveData)
import System.Environment (getArgs)

main :: IO ()
main = do
    [h, e] <- getArgs
    runSecureClient host port (path h e) application

application :: ClientApp ()
application connection = forever .> void <| do
    json <- receiveData connection
    let message = eitherDecode json
    print (message :: Either String Message)

host :: String
host = "wss.redditmedia.com"

port :: PortNumber
port = 443

path :: String -> String -> String
path h e = encodePath segments query |> toLazyByteString |> unpack where
    segments =
        [ "thebutton"
        ]
    query =
        [ ("h", Just (pack h))
        , ("e", Just (pack e))
        ]
