{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PartialTypeSignatures, TemplateHaskell #-}

--
-- This app connects a single user account on hipchat with the bot, so
-- the bot speaks on hipchat as if they are the user. Handy for testing.
-- look at my more recent hs-hipchat-to-websocket package for a better approach.
--

import           Prelude              hiding (print)
import qualified Network.WebSockets   as WS
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Data.Default         (Default, def)
import           Control.Concurrent 
import           Control.Applicative  ((<|>),(<$>),(<*>))
import           Control.Monad.Trans  (liftIO)
import           Control.Monad        (forever, forM_, forM)
import qualified Control.Exception    as E
import           System.Environment   (getArgs)
import           Text.Read            (readMaybe)
import qualified Data.Map             as M
import           Data.Monoid          ((<>))
import           Data.List            (lookup,any)
import           Data.Aeson           ((.=),object,encode,decode,toJSON)
import qualified System.IO            as IO
import           System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

import qualified Data.ByteString.Lazy.Char8  as BL

import           Network.Wreq
import           Control.Lens         hiding ((.=))
import           Data.Aeson.Lens
import qualified Web.Scotty           as W;

import           App.Args
import           App.Messages
import           App.Format

--
-- lensy param struct for top level args
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
    printLn "Starting Client" ()

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
        Left err -> printLn "Error: {}" (Only err)
        Right p -> getRooms p


getRooms :: Params -> IO ()
getRooms params = do

    -- extract tuples of (roomid, roomname) from room JSON:
    let authParam = defaults & param "auth_token" .~ [params^.hipchatToken]
                             & header "Content-Type" .~ ["application/json"]

    roomResp <- getWith authParam $ T.unpack (params^.hipchatAddress) <> "/v2/room"
    let rs = roomResp ^.. responseBody 
                        . key "items" 
                        . _Array 
                        . traverse 
                        . to (\o -> (o ^?! key "id" . _Integral,  o ^?! key "name" . _String) )

    --get list of all roomIds if no room provided, else look for room provided and return that.
    let roomIds = case params^.roomName of
            "" -> rs ^.. traverse . _1
            str -> rs ^.. traverse . filtered (\a -> snd a == str) . _1

    printLn "listening in room IDs: {}" (Only $ show roomIds)

    --run the chat loop with the room ids found
    case roomIds of
        [] -> printLn "don't know about room '{}'" (Only $ params^.roomName)
        _  -> chatloop params roomIds authParam


chatloop :: Params -> [Int] -> Options -> IO ()
chatloop params roomIds authParam = do

    let hipchatAddyStr = T.unpack (params^.hipchatAddress)
        thisUrl = "http://" <> (params^.thisAddress) <> ":" <> (T.pack $ show (params^.thisPort))
        serverAddressStr = T.unpack (params^.serverAddress)
        serverPortNum = params^.serverPort

    --create a socket connection per room. leave each socket on a loop
    --messages get from hipchat to server by being put in an mvar. messages
    roomLinks <- forM roomIds $ \id -> do

        mv <- newEmptyMVar
        forkIO $ WS.runClient serverAddressStr serverPortNum "/" $ \conn -> do

            let addy = hipchatAddyStr <> "/v2/room/" <> (show id) <> "/message"
                postMessage text =
                    -- for some reason hipchat doesnt respond so fork it and catch
                    -- the timeout...
                    let fireAndForget = forkIO $ flip E.catch ((\(e :: E.SomeException) -> return ())) $ do
                            postWith authParam addy $ toJSON $ object ["message" .= text]
                            return ()
                    in fireAndForget >> return ()

            --fork a thread to write responses to hipchat from the server
            liftIO $ forkIO $ forever $ do
                resp <- WS.receiveData conn
                liftIO $ case (decode resp :: Maybe ServerMessage) of
                    Just message -> do
                        printLn "@JamesBot: {}" (Only $ message^.sMessage)
                        postMessage $ message^.sMessage
                    Nothing -> printLn "Odd json from server: {}" (Only resp)

            --if the mvar gets filled with a ClientMessage, send to server
            forever $ do
                cm <- takeMVar mv
                WS.sendTextData conn $ encode cm

        return (id,mv)

    --store IDs of webhooks we create so we can filter on them and
    --clean them up on finish:
    webhookIds <- newMVar [] :: IO (MVar [(Int,Int)])
    done <- newEmptyMVar

    let cleanUp = do
            ids <- takeMVar webhookIds
            printLn "Received kill signal, removing webhook IDs {}" (Only $ show $ ids ^.. traverse . _2)
            forM_ ids $ \(roomId,hookId) -> do
                deleteWith authParam (hipchatAddyStr <> "/v2/room/" <> (show roomId) <> "/webhook/" <> (show hookId))
            putMVar done ()

    installHandler sigINT (Catch $ cleanUp) Nothing
    installHandler sigTERM (Catch $ cleanUp) Nothing

    --kick off a web server to listen for messages. when they arrive, find
    --the mvar with matching room ID so that we target the correct socket connection,
    --and send to the backend. This ensures that responses will go back to the right room.
    forkIO $ W.scotty (params^.thisPort) $ do

        let doParse b = do
                
                let roomId = b ^?! key "item" . key "room" . key "id" . _Integral
                    cmsg = def 
                        & cName .~ ("@" <> (b ^?! key "item" . key "message" . key "from" . key "mention_name" . _String))
                        & cMessage .~ (b ^?! key "item" . key "message" . key "message" . _String)
                        & cRoom .~ (b ^?! key "item" . key "room" . key "name" . _String)
    
                case lookup roomId roomLinks of
                    Just mv -> do
                        printLn "{} ({}): {}" (cmsg^.cName, cmsg^.cRoom, cmsg^.cMessage)
                        liftIO $ putMVar mv cmsg
                    Nothing -> printLn "room ID {} unknown to me" (Only roomId)
    
                W.text "Thanks!"

        W.post "/message" $ do
            b <- W.body
            let thisHookId = b ^?! key "webhook_id" . _Integral

            --ignore data sent to webhooks we didnt register just now:
            validHooks <- liftIO $ readMVar webhookIds
            if any (\a -> snd a == thisHookId) validHooks then doParse b else W.text "Ignoring"

        W.matchAny "/" $ W.text "Lark"


    --register webhook to receive messages for each room we are in
    --this will start data flowing to the web server above, which will in
    --turn start pushing data into mvars to be sent to our bot server
    forM_ roomIds $ \roomId -> do
        let whUrl = hipchatAddyStr <> "/v2/room/" <> (show roomId) <> "/webhook"
        r <- postWith authParam whUrl $
            toJSON $ object [
                "url" .= (thisUrl <> "/message"),
                "pattern" .= (".*" :: T.Text),
                "event" .= ("room_message" :: T.Text),
                "name" .= ("messagehook" :: T.Text)
            ]

        case (r ^? responseBody . key "id" . _Integral) of
            Just hookId -> do
                hookIds <- takeMVar webhookIds
                putMVar webhookIds ((roomId,hookId):hookIds) 
            Nothing -> BL.putStrLn $ "Webhook id can't be obtained from: " <> (r^.responseBody)

    --prevent exit of thread until "done"
    readMVar done
    return ()




