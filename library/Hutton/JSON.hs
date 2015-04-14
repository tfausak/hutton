{-# LANGUAGE OverloadedStrings #-}

module Hutton.JSON where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON, Value (..), parseJSON, (.:))
import Data.Text (Text)

data Message = Message
    { message_payload :: Payload
    , message_type :: Text
    } deriving (Read, Show)

instance FromJSON Message where
    parseJSON (Object o) = Message
        <$> o .: "payload"
        <*> o .: "type"
    parseJSON _ = mzero

data Payload = Payload
    { payload_nowStr :: Text
    , payload_participantsText :: Text
    , payload_secondsLeft :: Double
    , payload_tickMac :: Text
    } deriving (Read, Show)

instance FromJSON Payload where
    parseJSON (Object o) = Payload
        <$> o .: "now_str"
        <*> o .: "participants_text"
        <*> o .: "seconds_left"
        <*> o .: "tick_mac"
    parseJSON _ = mzero
