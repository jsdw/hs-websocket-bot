{-# LANGUAGE OverloadedStrings #-}

module App.Rules where

import           Prelude                    hiding (log)
import           App.Brain
import           App.Messages
import           Data.Attoparsec.Text
import           Control.Monad.Trans        (liftIO)
import qualified Data.Text                  as T
import           Data.Text                  (append, pack)


--
-- build the brain here. this can include IO responses
-- and whatnot that'll be used when the time comes
--
buildRules :: BotBrainBuilder
buildRules = do

    addResponse (ParserRule $ string "count" >> return ()) $ do
        count <- getCount
        writeMessage $ "Messages seen: " `append` (pack $ show count)

    addResponse (ParserRule $ string "hello" >> return ()) $ do
        name <- getName
        log $ name `append` " said hi"
        writeMessage $ "Hi " `append` name

    addResponse EveryTime $ do
        writeMessage "pong!"

    return ()