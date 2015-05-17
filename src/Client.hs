import           Prelude             hiding (print)
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
import           Control.Lens         hiding ((.=))

import           App.Args
import           App.Messages
import           App.Format

botPrefix =  " Bot> " :: T.Text
userPrefix = "User> " :: T.Text

application conn = do

    --fork a thread to write responses back from the bot
    liftIO $ forkIO $ forever $ do
        resp <- WS.receiveData conn
        let maybeMess = decode resp :: Maybe ServerMessage
        printLn "" ()
        case maybeMess of
            Just message -> printLn "{} {}" (botPrefix, message^.sMessage)
            Nothing -> printLn "{} what on earth was that?!" (Only botPrefix) 
        print "{}" (Only userPrefix)

    --send data to the bot in a continuous loop 
    forever $ do
        print "{}" (Only userPrefix)
        input <- getLine
        WS.sendTextData conn $ encode $ ClientMessage "James" "terminal" (T.pack input)


main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    printLn "Starting Client" ()

    argMap <- fmap parseKeys getArgs

    --parse port number from args
    let (Just port) = (maybeP >>= readMaybe :: Maybe Int) <|> Just 9090
            where maybeP = M.lookup "port" argMap <|> M.lookup "p" argMap

    --parse address from args
    let (Just address) =
            M.lookup "address" argMap <|> M.lookup "a" argMap <|> Just "0.0.0.0"

    withSocketsDo $ WS.runClient address port "/" application >> return ()