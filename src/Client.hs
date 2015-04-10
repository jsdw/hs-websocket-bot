{-# LANGUAGE OverloadedStrings #-}

import qualified Network.WebSockets  as WS
import           Network.Socket      (withSocketsDo) --only really necessary for windows
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Control.Concurrent
import qualified Control.Exception   as E
import           Control.Applicative ((<|>),(<$>),(<*>))
import           Control.Monad.Trans (liftIO)
import           Control.Monad       (forever)
import           System.Environment  (getArgs)
import           Text.Read           (readMaybe)
import qualified Data.Map            as M
import           Data.Aeson          ((.=),object,encode,decode)
import qualified System.IO           as IO

import           App.Args
import           App.Messages

botPrefix =  " Bot> "
userPrefix = "User> "

application conn = do

    --fork a thread to write responses back from the bot
    liftIO $ forkIO $ forever $ do
        resp <- WS.receiveData conn
        let maybeMess = decode resp :: Maybe ServerMessage
        T.putStrLn ""
        case maybeMess of
            Just message -> T.putStrLn $ botPrefix `T.append` sMessage message
            Nothing -> do
                T.putStrLn $ botPrefix `T.append` "what on earth was that?!"
        T.putStr userPrefix

    --send data to the bot in a continuous loop 
    forever $ do
        T.putStr userPrefix
        input <- getLine
        WS.sendTextData conn $ encode $ ClientMessage "James" (T.pack input)


main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    putStrLn "Starting Client"

    argMap <- fmap parseKeys getArgs

    --parse port number from args
    let (Just port) = (maybeP >>= readMaybe :: Maybe Int) <|> Just 9090
          where maybeP = M.lookup "port" argMap <|> M.lookup "p" argMap

    --parse address from args
    let (Just address) =
          M.lookup "address" argMap <|> M.lookup "a" argMap <|> Just "0.0.0.0"

    --this runs our app:
    let runApp = withSocketsDo $ WS.runClient address port "/" application >> return ()

    --run our app
    runApp