{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forever, mzero, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, Value (Object), eitherDecode, parseJSON, (.:))
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Char (isDigit)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (intersperse)
import Data.String (fromString)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Flow ((|>), (<|))
import Network.HTTP.Client (Manager, Request)
import Network.HTTP.Conduit (httpLbs, parseUrl, requestHeaders, responseBody,
    simpleHttp, urlEncodedBody, withManager)
import Network.WebSockets (Connection, receiveData)
import Rainbow (Chunk, Radiant, blue, cyan, fore, green, magenta, putChunk,
    red, yellow, (<>))
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Regex (matchRegex, mkRegex)
import Wuss (runSecureClient)

import Prelude

-- Get command-line arguments.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [threshold', username, password] -> main1 threshold' username password
        _ -> do
            putStrLn "Usage: hutton THRESHOLD USERNAME PASSWORD"
            exitFailure

-- Set up login request.
main1 :: String -> String -> String -> IO ()
main1 threshold' username password = do
    let threshold = read threshold' :: Int

    putStrLn "Logging in..."
    initialLoginRequest <- parseUrl "https://www.reddit.com/api/login"
    let loginQuery =
            [ ("api_type", "json")
            , ("user", fromString username)
            , ("passwd", fromString password)
            ]
        loginRequest = urlEncodedBody loginQuery initialLoginRequest
    withManager (main2 threshold loginRequest)

-- Actually log in.
main2 :: (MonadIO m) => Int -> Request -> Manager -> m ()
main2 threshold loginRequest manager = do
    loginResponse <- httpLbs loginRequest manager
    liftIO <| case eitherDecode (responseBody loginResponse) of
        Right login -> main3 threshold login manager
        Left message -> do
            putStrLn message
            exitFailure

-- Get query parameters.
main3 :: Int -> LoginResponse -> Manager -> IO ()
main3 threshold login manager = do
    putStrLn "Logged in."
    putStrLn ("    cookie = " ++ cookie login)
    putStrLn ("    modhash = " ++ modhash login)

    putStrLn "Getting query parameters..."
    buttonResponse <- simpleHttp "https://www.reddit.com/r/thebutton"
    let needle = mkRegex "\\?h=([0-9a-f]+)&e=([0-9]+)"
        haystack = unpack buttonResponse
    case matchRegex needle haystack of
        Just [h, e] -> main4 h e threshold login manager
        _ -> do
            putStrLn "Failed to get query parameters."
            exitFailure

-- Connect to WebSocket.
main4 :: String -> String -> Int -> LoginResponse -> Manager -> IO ()
main4 h e threshold login manager = do
    putStrLn "Got query parameters."
    putStrLn ("    h = " ++ h)
    putStrLn ("    e = " ++ e)

    putStrLn "Connecting to WebSocket..."
    let host = "wss.redditmedia.com"
        port = 443
        path = "/thebutton?h=" ++ h ++ "&e=" ++ e
    runSecureClient host port path (main5 threshold login manager)

-- Set up WebSocket client.
main5 :: Int -> LoginResponse -> Manager -> Connection -> IO ()
main5 threshold login manager connection = do
    putStrLn "Connected to WebSocket."
    ref <- newIORef 0
    forever (main6 connection ref threshold login manager)

-- Listen for messages.
main6 :: Connection -> IORef Int -> Int -> LoginResponse -> Manager -> IO ()
main6 connection ref threshold login manager = do
    value <- receiveData connection
    case eitherDecode value of
        Right datum -> main7 datum ref threshold login manager
        Left message -> putStrLn message

-- Figure out what to do with messages.
main7 :: Datum -> IORef Int -> Int -> LoginResponse -> Manager -> IO ()
main7 datum ref threshold login manager = do
    let thisNumPressers = numPressers datum
    lastNumPressers <- readIORef ref
    when (thisNumPressers > lastNumPressers) (main8 ref thisNumPressers)
    prettyPrint datum
    when (secondsLeft datum <= threshold) (main9 datum login manager)

-- Update the number of pressers.
main8 :: IORef Int -> Int -> IO ()
main8 ref thisNumPressers = do
        writeIORef ref thisNumPressers
        putStrLn ""

-- Press the button!
main9 :: Datum -> LoginResponse -> Manager -> IO ()
main9 datum login manager = do
    putStrLn "Pressing the button..."
    initialPressRequest <- parseUrl "https://www.reddit.com/api/press_button"
    -- All of this probably isn't necessary. I don't have enough throwaway
    -- accounts to figure what the minimal set of parameters is.
    let pressQuery =
            [ ("prev_seconds", datum |> secondsLeft |> show |> fromString)
            , ("r", "thebutton")
            , ("renderstyle", "html")
            , ("seconds", datum |> secondsLeft |> show |> fromString)
            , ("tick_mac", datum |> mac |> fromString)
            , ("tick_time", datum |> datum_payload |> payload_nowStr |> fromString)
            , ("uh", login |> modhash |> fromString)
            ]
        pressHeaders =
            [ ("Accept", "application/json, text/javascript, */*; q=0.01")
            , ("Content-Type", "application/x-www-form-urlencoded; charset=UTF-8")
            , ("Cookie", "reddit_session=" <> (login |> cookie |> fromString))
            , ("Origin", "http://www.reddit.com")
            , ("Referer", "http://www.reddit.com/r/thebutton/")
            , ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.90 Safari/537.36")
            , ("X-Requested-With", "XMLHttpRequest")
            ]
        pressRequest = urlEncodedBody pressQuery initialPressRequest
            { requestHeaders = pressHeaders
            }
    pressResponse <- httpLbs pressRequest manager
    print pressResponse
    putStrLn "Pressed the button."
    exitSuccess

data LoginResponse = LoginResponse
    { login_json :: LoginJson
    } deriving (Show)

instance FromJSON LoginResponse where
    parseJSON (Object o) = LoginResponse
        <$> o .: "json"
    parseJSON _ = mzero

data LoginJson = LoginJson
    { login_data :: LoginData
    } deriving (Show)

instance FromJSON LoginJson where
    parseJSON (Object o) = LoginJson
        <$> o .: "data"
    parseJSON _ = mzero

data LoginData = LoginData
    { login_cookie :: String
    , login_modhash :: String
    } deriving (Show)

instance FromJSON LoginData where
    parseJSON (Object o) = LoginData
        <$> o .: "cookie"
        <*> o .: "modhash"
    parseJSON _ = mzero

cookie :: LoginResponse -> String
cookie response = response
    |> login_json
    |> login_data
    |> login_cookie

modhash :: LoginResponse -> String
modhash response = response
    |> login_json
    |> login_data
    |> login_modhash

data Datum = Datum
    { datum_payload :: Payload
    , datum_type :: String
    } deriving (Show)

instance FromJSON Datum where
    parseJSON (Object o) = Datum
        <$> o .: "payload"
        <*> o .: "type"
    parseJSON _ = mzero

data Payload = Payload
    { payload_nowStr :: String
    , payload_participantsText :: String
    , payload_secondsLeft :: Float
    , payload_tickMac :: String
    } deriving (Show)

instance FromJSON Payload where
    parseJSON (Object o) = Payload
        <$> o .: "now_str"
        <*> o .: "participants_text"
        <*> o .: "seconds_left"
        <*> o .: "tick_mac"
    parseJSON _ = mzero

prettyPrint :: Datum -> IO ()
prettyPrint datum = do
    mapM_ putChunk (format datum)
    putStrLn ""

format :: Datum -> [Chunk]
format datum = intersperse "\t"
    [ (datum |> secondsLeft |> show |> fromString) <> (datum |> color |> fore)
    , datum |> numPressers |> show |> fromString
    , datum |> time |> show |> fromString
    , datum |> mac |> fromString
    ]

secondsLeft :: Datum -> Int
secondsLeft datum = datum
    |> datum_payload
    |> payload_secondsLeft
    |> floor

color :: Datum -> Radiant
color datum = datum |> secondsLeft |> go where
    go x
        | x > 51 = magenta -- purple
        | x > 41 = blue
        | x > 31 = green
        | x > 21 = yellow
        | x > 11 = cyan -- orange
        | otherwise = red

numPressers :: Datum -> Int
numPressers datum = datum
    |> datum_payload
    |> payload_participantsText
    |> filter isDigit
    |> read

time :: Datum -> UTCTime
time datum = datum
    |> datum_payload
    |> payload_nowStr
    |> parseTimeOrError False defaultTimeLocale "%Y-%m-%d-%H-%M-%S"

mac :: Datum -> String
mac datum = datum
    |> datum_payload
    |> payload_tickMac
