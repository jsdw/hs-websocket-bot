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
    cMessage :: Text    
} deriving (Show)

instance FromJSON ClientMessage where
    parseJSON (Object v) = ClientMessage <$> v .: "name" <*> v .: "message"
    parseJSON _ = mzero

instance ToJSON ClientMessage where
    toJSON (ClientMessage name msg) = object ["name" .= name, "message" .= msg]


-- message sent from the server to a client
data ServerMessage = ServerMessage {
    sMessage :: Text
} deriving (Show)

instance FromJSON ServerMessage where
    parseJSON (Object v) = ServerMessage <$> v .: "message"
    parseJSON _ = mzero

instance ToJSON ServerMessage where
    toJSON (ServerMessage msg) = object ["message" .= msg]

instance Default ServerMessage where
    def = ServerMessage ""

