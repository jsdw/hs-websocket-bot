{-# LANGUAGE OverloadedStrings #-}

module App.Messages (

    ClientMessage(..),
    ServerMessage(..),

    --lenses
    cName,
    cRoom,
    cMessage,

    sMessage,
    sRoom

) where

import           Data.Aeson
import           Control.Applicative ((<$>),(<*>))
import           Control.Monad       (mzero)
import           Control.Lens        hiding ((.=))
import           Data.Text           (Text)
import           Data.Default        (def,Default)

-- message sent to the server from the client
data ClientMessage = ClientMessage {
    _cName :: Text,
    _cRoom :: Text,
    _cMessage :: Text   
} deriving (Show)

makeLenses ''ClientMessage

instance FromJSON ClientMessage where
    parseJSON (Object v) = ClientMessage 
        <$> v .: "name" 
        <*> v .: "room" 
        <*> v .: "message"
    parseJSON _ = mzero

instance Default ClientMessage where 
    def = ClientMessage "" "" ""

instance ToJSON ClientMessage where
    toJSON (ClientMessage name room msg) = object [
            "name" .= name, 
            "message" .= msg,
            "room" .= room
        ]


-- message sent from the server to a client
data ServerMessage = ServerMessage {
    _sMessage :: Text,
    _sRoom :: Text
} deriving (Show)

makeLenses ''ServerMessage

instance Default ServerMessage where 
    def = ServerMessage "" ""

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


