{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PartialTypeSignatures, TemplateHaskell #-}

module App.Args (parse, parseKeys, ignoreKeys, runParamExtractor, getMaybe, getMaybeDef, setParam, E.throwError) where

import qualified Data.List                 as L
import qualified Data.Map                  as M
import           Data.Default
import           Control.Lens
import           Control.Monad.Except      as E
import           Control.Monad.Trans.State as S


--very simple, just split args into keyed and "other", maintaining order of other.
--keyed args begin with - or --. no protection against supplying args we dont want.
parse args = parseArgs args M.empty []
parseKeys args = fst $ parse args
ignoreKeys args = snd $ parse args

parseArgs [] keyed other = (keyed, other)
parseArgs (('-':'-':key):val:rest) keyed other = parseArgs rest (M.insert key val keyed) other
parseArgs (('-':key):val:rest) keyed other = parseArgs rest (M.insert key val keyed) other
parseArgs (val:rest) keyed other = parseArgs rest keyed (other ++ [val])

--
-- a quick error+state monad to help extract args into a structure of your choosing,
-- returning Left String if error or Right structure if success
--
type ParamExtractor p = StateT (M.Map String String,p) (Except String)

runParamExtractor :: Default p => M.Map String String -> ParamExtractor p a -> Either String p
runParamExtractor args m = fmap snd $ runExcept $ execStateT m (args,def)

--handle arg extracting
getMaybe err []     = E.throwError err
getMaybe err (s:ss) = do
    args <- fmap fst S.get
    case M.lookup s args of
        Nothing -> getMaybe err ss
        Just a  -> return a

setParam l a = assign (_2.l) a

getMaybeDef err ss d = getMaybe err ss `E.catchError` (\_ -> return d)