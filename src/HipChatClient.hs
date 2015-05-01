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

chatloop hipchat authParam roomIds otherArgs = do

    --parse port number of the websocket server from args
    let (Just serverPort) = (maybeP >>= readMaybe :: Maybe Int) <|> Just 9090
          where maybeP = M.lookup "port" otherArgs <|> M.lookup "p" otherArgs

    --parse address of the websocket server from args
    let (Just serverAddress) =
          M.lookup "address" otherArgs <|> M.lookup "a" otherArgs <|> Just "127.0.0.1"

    --parse incoming port number (for hipchat webhooks) from args
    let (Just incomingPort) =  M.lookup "incoming-port" otherArgs <|> M.lookup "ip" otherArgs <|> Just "8080"
    let mIncomingAddress =  M.lookup "incoming-address" otherArgs <|> M.lookup "ia" otherArgs <|> Nothing


    let localAddress = "http://PUTADDRESSHERE:"++incomingPort

    --kick off a web server to listen for messages

    --register webhook to receive messages for each room we are in
    forM_ roomIds $ \id -> do
        postWith authParam (hipchat++"/v2/room/"++(show id)++"/webhook") $
            toJSON $ object [
                "url" .= (T.pack localAddress `mappend` "/message" :: T.Text),
                "event" .= ("room_message" :: T.Text),
                "name" .= ("messagehook" :: T.Text)
            ]


    --send test message to desired room(s)
    forM_ roomIds $ \id -> do
        postWith authParam (hipchat++"/v2/room/"++(show id)++"/message") $
            toJSON $ object ["message" .= ("Testing" :: T.Text)]

    return ()


app hipchat token otherArgs = do

    --parse room to enter if provided
    let mRoom = M.lookup "room" otherArgs <|> M.lookup "r" otherArgs

    -- extract tuples of (roomid, roomname) from room JSON:
    let authParam = defaults & param "auth_token" .~ [T.pack token]
                             & header "Content-Type" .~ ["application/json"]
    roomResp <- getWith authParam (hipchat++"/v2/room")
    let rs = roomResp ^.. responseBody 
                        . key "items" 
                        . _Array 
                        . traverse 
                        . to (\o -> (o ^?! key "id" . _Integral,  o ^?! key "name" . _String) )

    --get list of all roomIds if no room provided, else look for room provided and return that.
    let roomIds = case mRoom of
         Nothing -> rs ^.. traverse . _1
         Just str -> rs ^.. traverse . filtered (\a -> snd a == T.pack str) . _1

    --run the chat loop with the room ids found
    case roomIds of
        [] -> putStrLn "must provide a valid room name with --room or -r"
        _  -> chatloop hipchat authParam roomIds otherArgs




-- Handle our params with a little helper monad, below:
--
-- lensy param struct
--
data Params = Params {
    _hipchatToken :: T.Text,
    _hipchatAddress :: T.Text,
    _serverAddress :: T.Text,
    _serverPort :: Int,
    _thisUrl :: T.Text,
    _roomName :: T.Text
} deriving (Show)

instance Default Params where
    def = Params {
        _hipchatToken = "",
        _hipchatAddress = "",
        _serverAddress = "",
        _serverPort = 9090,
        _thisUrl = "",
        _roomName = ""
    }

makeLenses ''Params



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

         sa <- getMaybeDef "must provide websocket server address with --server-address or -sa" ["server-address", "sa"] "127.0.0.1"
         sp <- getMaybeDef "must provide websocket server address with --server-address or -sa" ["server-port", "sp"] "9090"

         let serverPortInt = readMaybe sp
         case serverPortInt of
            Nothing -> throwError "server port not a valid int"
            Just p -> setParam serverPort p

         setParam serverAddress (T.pack sa)
         
         ta <- getMaybe "must provide a hipchat auth token with --this-url or -tu" ["this-url", "tu"]

         setParam thisUrl (T.pack ta)

    --print error if we end up with one after extracting params, else
    --pass params to app
    case eParams of
        Left err -> putStrLn err
        Right p -> undefined

    return ()
