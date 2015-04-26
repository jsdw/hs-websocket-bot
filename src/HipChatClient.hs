{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PartialTypeSignatures #-}

import qualified Network.WebSockets   as WS
import           Network.Socket       (withSocketsDo) --only really necessary for windows
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Data.Default         (def)
import           Control.Concurrent 
import qualified Control.Exception    as E
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

import           App.Args
import           App.Messages

chatloop hipchat authParam roomIds otherArgs = do

    --parse port number from args
    let (Just port) = (maybeP >>= readMaybe :: Maybe Int) <|> Just 9090
          where maybeP = M.lookup "port" otherArgs <|> M.lookup "p" otherArgs

    --parse address from args
    let (Just address) =
          M.lookup "address" otherArgs <|> M.lookup "a" otherArgs <|> Just "0.0.0.0"

    --parse incoming port number (for hipchat webhooks) from args
    let (Just incomingPort) = (maybeP >>= readMaybe :: Maybe Int) <|> Just 8080
          where maybeP = M.lookup "incoming-port" otherArgs <|> M.lookup "ip" otherArgs

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


main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    putStrLn "Starting Client"

    argMap <- fmap parseKeys getArgs

    --parse hipchat address from args
    let mHipchatAddress = M.lookup "hipchat" argMap <|> M.lookup "h" argMap

    --auth token for hipchat API
    let mToken = M.lookup "token" argMap <|> M.lookup "t" argMap

    case mToken of
        Just token -> case mHipchatAddress of
            Just hipchatAddress -> app hipchatAddress token argMap
            Nothing -> putStrLn "must provide a hipchat server address using --hipchat or -h"
        Nothing -> putStrLn "must provide a hipchat auth token with --token or -t"

