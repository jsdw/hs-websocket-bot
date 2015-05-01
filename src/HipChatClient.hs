{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PartialTypeSignatures, TemplateHaskell #-}

import qualified Network.WebSockets   as WS
import           Network.Socket       (withSocketsDo) --only really necessary for windows
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Data.Default         (Default, def)
import           Control.Concurrent 
import           Control.Applicative  ((<|>),(<$>),(<*>))
import           Control.Monad.Trans  (liftIO)
import           Control.Monad        (forever, forM_)
import           System.Environment   (getArgs)
import           Text.Read            (readMaybe)
import qualified Data.Map             as M
import           Data.Aeson           ((.=),object,encode,decode,toJSON)
import qualified System.IO            as IO

import           Network.Wreq
import           Control.Lens         hiding ((.=))
import           Data.Aeson.Lens
import qualified Web.Scotty           as W;

import           App.Args
import           App.Messages

--
-- lensy param struct
--
data Params = Params {
    _hipchatToken :: T.Text,
    _hipchatAddress :: T.Text,
    _serverAddress :: T.Text,
    _serverPort :: Int,
    _thisAddress :: T.Text,
    _thisPort :: Int,
    _roomName :: T.Text
} deriving (Show)

instance Default Params where
    def = Params {
        _hipchatToken = "",
        _hipchatAddress = "",
        _serverAddress = "127.0.0.1",
        _serverPort = 9090,
        _thisAddress = "",
        _thisPort = 8080,
        _roomName = ""
    }

makeLenses ''Params


main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    putStrLn "Starting Client"

    argMap <- fmap parseKeys getArgs

    --parse args here, returning error if something not properly set
    let eParams = runParamExtractor argMap $ do

         ha <- getMaybe "must provide a hipchat server address using --hipchat or -h" ["hipchat", "h"]
         ht <- getMaybe "must provide a hipchat auth token with --token or -t" ["token", "t"]

         setParam hipchatAddress (T.pack ha)
         setParam hipchatToken (T.pack ht)

         sa <- getMaybeDef ["server-address", "sa"] "127.0.0.1"
         sp <- getMaybeDef ["server-port", "sp"] "9090"

         let serverPortInt = readMaybe sp
         case serverPortInt of
            Nothing -> throwError "server port not a valid int"
            Just p -> setParam serverPort p

         setParam serverAddress (T.pack sa)
         
         ta <- getMaybe "must provide this address --this-address or -ta" ["this-address", "ta"]
         tp <- getMaybe "must provide this port --this-port or -tp" ["this-port", "tp"]

         let thisPortInt = readMaybe tp
         case thisPortInt of
            Nothing -> throwError "this port not a valid int"
            Just p -> setParam thisPort p

         rn <- getMaybe "must provide a room name with --room or -r" ["room", "r"]

         setParam thisAddress (T.pack ta)
         setParam roomName (T.pack rn)

    --print error if we end up with one after extracting params, else
    --pass params to next stage
    case eParams of
        Left err -> putStrLn $ "Error: "++err
        Right p -> getRooms p


getRooms :: Params -> IO ()
getRooms params = do

    -- extract tuples of (roomid, roomname) from room JSON:
    let authParam = defaults & param "auth_token" .~ [params^.hipchatToken]
                             & header "Content-Type" .~ ["application/json"]

    roomResp <- getWith authParam $ T.unpack (params^.roomName) ++ "/v2/room"
    let rs = roomResp ^.. responseBody 
                        . key "items" 
                        . _Array 
                        . traverse 
                        . to (\o -> (o ^?! key "id" . _Integral,  o ^?! key "name" . _String) )

    --get list of all roomIds if no room provided, else look for room provided and return that.
    let roomIds = case params^.roomName of
         "" -> rs ^.. traverse . _1
         str -> rs ^.. traverse . filtered (\a -> snd a == str) . _1

    --run the chat loop with the room ids found
    case roomIds of
        [] -> T.putStrLn $ "room provided is not known: " `mappend` (params^.roomName)
        _  -> chatloop params roomIds authParam


chatloop :: Params -> [Int] -> Options -> IO ()
chatloop params roomIds authParam = do

    let hipchatAddyStr = T.unpack (params^.hipchatAddress)
    let thisUrl = (params^.thisAddress) `mappend` (T.pack $ show (params^.thisPort))

    --kick off a web server to listen for messages


    --register webhook to receive messages for each room we are in
    forM_ roomIds $ \id -> do
        postWith authParam (hipchatAddyStr++"/v2/room/"++(show id)++"/webhook") $
            toJSON $ object [
                "url" .= (thisUrl `mappend` "/message"),
                "event" .= ("room_message" :: T.Text),
                "name" .= ("messagehook" :: T.Text)
            ]


    --send test message to desired room(s)
    forM_ roomIds $ \id -> do
        postWith authParam (hipchatAddyStr++"/v2/room/"++(show id)++"/message") $
            toJSON $ object ["message" .= ("Testing" :: T.Text)]

    return ()








