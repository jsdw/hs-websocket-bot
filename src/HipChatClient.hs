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
import           Control.Monad        (forever, forM_, forM)
import           System.Environment   (getArgs)
import           Text.Read            (readMaybe)
import qualified Data.Map             as M
import           Data.List            (lookup,any)
import           Data.Aeson           ((.=),object,encode,decode,toJSON)
import qualified System.IO            as IO

import qualified Data.ByteString.Lazy.Char8  as BL

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
    _roomName :: T.Text,
    _botName :: T.Text
} deriving (Show)

instance Default Params where
    def = Params {
        _hipchatToken = "",
        _hipchatAddress = "",
        _serverAddress = "0.0.0.0",
        _serverPort = 0,
        _thisAddress = "",
        _thisPort = 0,
        _roomName = "",
        _botName = ""
    }

makeLenses ''Params


main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    putStrLn "Starting Client"

    argMap <- fmap parseKeys getArgs

    --parse args here, returning error if something not properly set
    let eParams = runParamExtractor argMap $ do

         bn <- getMaybeDef ["bot-name", "bn"] "@james"

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

    roomResp <- getWith authParam $ T.unpack (params^.hipchatAddress) ++ "/v2/room"
    let rs = roomResp ^.. responseBody 
                        . key "items" 
                        . _Array 
                        . traverse 
                        . to (\o -> (o ^?! key "id" . _Integral,  o ^?! key "name" . _String) )

    --get list of all roomIds if no room provided, else look for room provided and return that.
    let roomIds = case params^.roomName of
         "" -> rs ^.. traverse . _1
         str -> rs ^.. traverse . filtered (\a -> snd a == str) . _1

    putStrLn $ "listening in room IDs: " ++ show roomIds

    --run the chat loop with the room ids found
    case roomIds of
        [] -> T.putStrLn $ "room provided is not known: " `mappend` (params^.roomName)
        _  -> chatloop params roomIds authParam


chatloop :: Params -> [Int] -> Options -> IO ()
chatloop params roomIds authParam = do

    let hipchatAddyStr = T.unpack (params^.hipchatAddress)
    let thisUrl = "http://" `mappend` (params^.thisAddress) `mappend` ":" `mappend` (T.pack $ show (params^.thisPort))

    let serverAddressStr = T.unpack (params^.serverAddress)
    let serverPortNum = params^.serverPort

    --create a socket connection per room. leave each socket on a loop
    --messages get from hipchat to server by being put in an mvar. messages
    roomLinks <- forM roomIds $ \id -> do

        mv <- newEmptyMVar
        forkIO $ WS.runClient serverAddressStr serverPortNum "/" $ \conn -> do

            let addy = hipchatAddyStr++"/v2/room/"++(show id)++"/message"
            let postMessage text = do
                 postWith authParam addy $ toJSON $ object ["message" .= text]
                 return ()

            --fork a thread to write responses to hipchat from the server
            liftIO $ forkIO $ forever $ do
                resp <- WS.receiveData conn
                liftIO $ case (decode resp :: Maybe ServerMessage) of
                    Just message -> do
                        T.putStrLn $ "sending from bot: " `mappend` (sMessage message)
                        postMessage $ sMessage message
                    Nothing -> T.putStrLn "Odd json from server!"

            --if the mvar gets filled with a ClientMessage, send to server
            forever $ do
                cm <- takeMVar mv
                WS.sendTextData conn $ encode cm

        return (id,mv)

    --kick off a web server to listen for messages. when they arrive, find
    --the mvar with matching room ID so that we target the correct socket connection,
    --and send to the backend. This ensures that responses will go back to the right room.
    webhookIds <- newMVar []
    forkIO $ W.scotty (params^.thisPort) $ do

        let doParse b = do

             liftIO $ BL.putStrLn $ "Data received: " `mappend` b 
             
             let roomId = b ^?! key "item" . key "room" . key "id" . _Integral
             let clientMsg = ClientMessage {
                 cName = "@" `mappend` (b ^?! key "item" . key "message" . key "from" . key "mention_name" . _String),
                 cMessage = b ^?! key "item" . key "message" . key "message" . _String
             }
 
             liftIO $ case lookup roomId roomLinks of
                 Just mv -> do
                     T.putStrLn $ (cName clientMsg) `mappend` " says: " `mappend` (cMessage clientMsg)
                     putMVar mv clientMsg
                 Nothing -> putStrLn $ "room ID "++(show roomId)++" is unknown to me"
 
             W.text "Thanks!"

        W.post "/message" $ do
            b <- W.body
            let thisHookId = b ^?! key "webhook_id" . _Integral

            --ignore data sent to webhooks we didnt register just now:
            validHooks <- liftIO $ readMVar webhookIds
            if any (== thisHookId) validHooks then doParse b else W.text "Ignoring"

        W.matchAny "/" $ W.text "Lark"


    --register webhook to receive messages for each room we are in
    --this will start data flowing to the web server above, which will in
    --turn start pushing data into mvars to be sent to our bot server
    forM_ roomIds $ \id -> do
        let whUrl = hipchatAddyStr++"/v2/room/"++(show id)++"/webhook"
        let whThis = thisUrl `mappend` "/message"
        r <- postWith authParam whUrl $
            toJSON $ object [
                "url" .= whThis,
                "pattern" .= (".*" :: T.Text),
                "event" .= ("room_message" :: T.Text),
                "name" .= ("messagehook" :: T.Text)
            ]

        case (r ^? responseBody . key "id" . _Integral) of
            Just id -> do
                hookIds <- takeMVar webhookIds
                putMVar webhookIds (id:hookIds) 
            Nothing -> BL.putStrLn $ "Webhook id can't be obtained from: " `mappend` (r^.responseBody)

    --prevent exit of thread
    let exitLoop = getLine >> exitLoop
    exitLoop





