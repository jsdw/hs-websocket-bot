module App.Brain where

import qualified Network.WebSockets    as WS
import           Data.Aeson  
import           Control.Applicative   ((<$>),(<*>))
import           Control.Monad         (mzero)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           Data.Default          (def,Default)
import           Control.Monad.Trans   (liftIO)
import           Control.Monad.State   (State(..), StateT(..), execState, execStateT, modify, get)
import qualified Data.Attoparsec.Text  as P
import           Control.Concurrent    (MVar,newMVar,modifyMVar_,readMVar)
import           Text.Regex.TDFA       ((=~))
import           System.Random         (randomIO,Random(..))

import           App.Messages

--
-- BOT STATE
--
-- bot state, singleton across everything.
--
-- lives in an MVar so multiple threads can update it.
--
type BotState = MVar BotStateInner
data BotStateInner = BotStateInner {
    bsMood :: Int,
    bsMessageCount :: Int
} deriving (Show)

instance Default BotStateInner where 
    def = BotStateInner {
        bsMood = 0,
        bsMessageCount = 0
    }

--
-- BOT BRAIN
--
-- state containing list of parsing things to try
-- and spontaneous message data, combined with actions
-- that will be performed on match
--
--
data BotBrain = BotBrain {
    rules :: [(BotBrainRule, BotBrainActionBuilder ())],
    spontaneous :: [(BotBrainTime, BotBrainActionBuilder ())]
}

instance Default BotBrain where
    def = BotBrain {
        rules = [],
        spontaneous = []
    }

data BotBrainRule = ParserRule (P.Parser ())
                  | RegexpRule String
                  | Exactly T.Text
                  | EveryTime
data BotBrainTime = Unknown

--
-- BOT BRAIN BUILDER
--
-- Build the bot brain (above) using this.
--
-- a simple state wrapper to allow building a botbrain,
-- with funcs to push rules and spontaneous bits
type BotBrainBuilder = State BotBrain ()

runBrainBuilder :: BotBrainBuilder -> BotBrain
runBrainBuilder brainScript = state { 
        rules = reverse $ rules state,
        spontaneous = reverse $ spontaneous state 
    }
    where state = execState brainScript def

addResponse rule action = modify $ \s -> 
    let r = rules s 
    in s{rules = ((rule,action):r)}

addSpontaneous time action = modify $ \s -> 
    let r = spontaneous s 
    in s{spontaneous = ((time,action):r)}

-- 
-- BOT BRAIN ACTION
--
-- a state provided to each action when it is run. This
-- enables the action to use the relevant connection and
-- get hold of the current message details
--
data BotBrainAction = BotBrainAction {
    actionMessage :: ClientMessage,
    actionConn :: WS.Connection,
    actionState :: BotState
}


-- provide a mechanism to build this BotBrainAction up,
-- with handy functions to make it easy.
type BotBrainActionBuilder a = StateT BotBrainAction IO a

runBrainActionBuilder :: BotBrainActionBuilder a -> BotBrainAction -> IO BotBrainAction
runBrainActionBuilder actionScript initialActionState = execStateT actionScript initialActionState

writeMessage :: T.Text -> BotBrainActionBuilder ()
writeMessage m = do
    state <- get
    let jsonResp = encode (def { sMessage = m } :: ServerMessage)
    liftIO $ WS.sendTextData (actionConn state) jsonResp

getMessage :: BotBrainActionBuilder T.Text
getMessage = get >>= return . cMessage . actionMessage 

getCount :: BotBrainActionBuilder Int
getCount = get >>= liftIO . readMVar . actionState >>= return . bsMessageCount

getName :: BotBrainActionBuilder T.Text
getName = get >>= return . cName . actionMessage 

random :: Random a => BotBrainActionBuilder a
random = liftIO $ randomIO

--
-- Given a botState and botBrain, generate a reponse
--
-- When we get a message, this finds the rule that
-- matches, runs the corresponding func, and potentially
-- sends a response.
--
generateResponse :: ClientMessage -> BotState -> BotBrain -> WS.Connection -> IO ()
generateResponse m botState botBrain conn =

    let initialActionState = BotBrainAction {
            actionMessage = m,
            actionConn = conn,
            actionState = botState
        }

        responseLoop st (rule:rs) = do

            --work out whether to continue by running
            --the parse rule against the message
            let bContinue = case fst rule of
                    ParserRule r -> case P.parseOnly r (cMessage m) of
                        Left _  -> False
                        Right _ -> True
                    RegexpRule s -> (T.unpack $ cMessage m) =~ s
                    Exactly s -> s == (cMessage m)
                    EveryTime -> True

            --either loop to the next rule or run the
            --action provided with it.
            if not bContinue
                then responseLoop st rs
                else runBrainActionBuilder (snd rule) st

    in do 
        --increment the message seen count
        modifyMVar_ botState $ \s -> return $ s{
                bsMessageCount = (bsMessageCount s) + 1
            }
        responseLoop initialActionState (rules botBrain) >> return ()







