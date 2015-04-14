{-# LANGUAGE OverloadedStrings #-}

module Hutton where

import Hutton.JSON (Message (..), Payload (..))
import Hutton.WebSockets (runSecureClient)

import Control.Monad (forever, void)
import Data.Aeson (eitherDecode)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.String (fromString)
import Flow
import Network.HTTP.Types (encodePath)
import Network.Socket (PortNumber)
import Network.WebSockets (ClientApp, receiveData)
import Rainbow (Chunk, Radiant, blue, cyan, fore, green, magenta, putChunkLn,
    red, yellow, (<>))
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let (h, e) = case args of
            [x, y] -> (x, y)
            _ -> ("1bfe495776935f5aaaa949c6152772a2d37525e3", "1429071395")
    runSecureClient host port (path h e) application

application :: ClientApp ()
application connection = forever .> void <| do
    json <- receiveData connection
    case eitherDecode json of
        Left e -> putStrLn e
        Right m -> m |> format |> putChunkLn

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

format :: Message -> Chunk
format message = fromString (show seconds) <> fore (color seconds) where
    seconds = message |> message_payload |> payload_secondsLeft |> floor

color :: Int -> Radiant
color n
    | n <= 11 = red
    | n <= 21 = cyan -- orange
    | n <= 31 = yellow
    | n <= 41 = green
    | n <= 51 = blue
    | otherwise = magenta
