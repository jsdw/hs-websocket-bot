{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module App.Rules where

import           Prelude                    hiding (log, print)
import           Data.Attoparsec.Text
import qualified Data.Text                  as T

import           App.Brain
import           App.Messages
import           App.Utility

--
-- build the brain here. this can include IO responses
-- and whatnot that'll be used when the time comes
--
buildRules :: BotBrainBuilder
buildRules = do

    addResponse (Exactly "random number" ) $ do
        (value :: Double) <- random 
        writeMessage $ format "Random double: {}" (Only value)

    addResponse (RegexpRule "lark.*" ) $ do
        writeMessage "so much lark."

    addResponse (ParserRule $ string "count" >> return ()) $ do
        count <- getCount
        writeMessage $ format "Messages seen: {}" (Only count)

    addResponse (ParserRule $ string "hello" >> return ()) $ do
        name <- getName
        print "{} said hi\n" (Only name)
        writeMessage $ format "Hi {}" (Only name)

    addResponse EveryTime $ do
        writeMessage "pong!"

    return ()