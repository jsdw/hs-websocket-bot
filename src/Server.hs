{-# LANGUAGE OverloadedStrings #-}

import qualified Network.WebSockets         as WS
import           Network.Socket             (withSocketsDo) --only really necessary for windows
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Data.Text.Encoding         as T
import qualified Data.ByteString.Lazy.Char8 as B
import           Control.Concurrent
import           Control.Monad.Trans        (liftIO)
import           Control.Monad              (forever,mzero)
import           Control.Applicative        ((<|>),(<$>),(<*>))
import qualified Control.Exception          as E
import           System.Environment         (getArgs)
import qualified Data.Map                   as M            
import           Text.Read                  (readMaybe)
import           Data.Aeson                 ((.=),FromJSON,ToJSON,parseJSON,toJSON,Object)
import           Data.Aeson                 as JSON
import           Data.Default               (def,Default)
       
import qualified App.Args                   as Args
import           App.Types                  as Types

--
-- When we get a message, this picks it up and can
-- respond however it likes
--
respondTo m state = do
	threadDelay 1000000
	return $ Just $ "Pong! " `T.append` (cMessage m)
	
--
-- The retrieve/reply loop
--
application state pending = flip E.finally disconnect $ do

	liftIO $ putStrLn "Connection established"
	conn <- WS.acceptRequest pending

	forever $ do 
		msg <- WS.receiveData conn

		let m = decode msg :: Maybe Types.ClientMessage
		case m of
			Just message -> do
				liftIO $ T.putStrLn $ "In:: " 
				           `T.append` (cName message) 
				           `T.append` ": " 
				           `T.append` (cMessage message)

				--fork a new thread so that we don't block
				--this loop if we want timers etc
				liftIO $ forkIO $ respondTo message state >>= writeResponse conn
				return ()

			Nothing -> B.putStrLn $ "Bad Input:: " `B.append` msg

	where 
		disconnect = liftIO $ putStrLn "Closing connection"
		writeResponse conn (Nothing) = return ()
		writeResponse conn (Just m) = WS.sendTextData conn $ encode (def { sMessage = m } :: Types.ServerMessage)

--
-- Our entry point
--
main = do
	putStrLn "Starting Websocket Bot Server"

	argMap <- fmap Args.parseKeys getArgs

	--bot state lives here
	state <- newMVar def :: IO (MVar Types.State)

	--parse port number from args
	let (Just port) = (maybeP >>= readMaybe :: Maybe Int) <|> Just 9090
		where maybeP = M.lookup "port" argMap <|> M.lookup "p" argMap

	--parse address from args
	let (Just address) =
		M.lookup "address" argMap <|> M.lookup "a" argMap <|> Just "0.0.0.0"

	putStrLn $ "port:    "++(show port)
	putStrLn $ "address: "++address

	withSocketsDo $ WS.runServer address port $ application state
