{-# LANGUAGE OverloadedStrings #-}

module App.Messages (

    ClientMessage(..),
    ServerMessage(..)

) where

import           Data.Aeson
import           Control.Applicative ((<$>),(<*>))
import           Control.Monad       (mzero)
import           Data.Text           (Text)
import           Data.Default        (def,Default)

-- message sent to the server from the client
data ClientMessage = ClientMessage {
    cName :: Text,
    cRoom :: Text,
    cMessage :: Text   
} deriving (Show)

instance FromJSON ClientMessage where
    parseJSON (Object v) = ClientMessage 
        <$> v .: "name" 
        <*> v .: "room" 
        <*> v .: "message"
    parseJSON _ = mzero

instance ToJSON ClientMessage where
    toJSON (ClientMessage name room msg) = object [
            "name" .= name, 
            "message" .= msg,
            "room" .= room
        ]


-- message sent from the server to a client
data ServerMessage = ServerMessage {
    sMessage :: Text,
    sRoom :: Text
} deriving (Show)

instance FromJSON ServerMessage where
    parseJSON (Object v) = ServerMessage 
        <$> v .: "message"
        <*> v .: "room"
    parseJSON _ = mzero

instance ToJSON ServerMessage where
    toJSON (ServerMessage msg room) = object [
            "message" .= msg,
            "room" .= room
        ]


