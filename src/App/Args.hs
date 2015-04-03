module App.Args (parse, parseKeys, ignoreKeys) where

import qualified Data.List as L
import qualified Data.Map  as M

--very simple, just split args into keyed and "other", maintaining order of other.
--keyed args begin with - or --. no protection against supplying args we dont want.
parse args = parseArgs args M.empty []
parseKeys args = fst $ parse args
ignoreKeys args = snd $ parse args

parseArgs [] keyed other = (keyed, other)
parseArgs (('-':'-':key):val:rest) keyed other = parseArgs rest (M.insert key val keyed) other
parseArgs (('-':key):val:rest) keyed other = parseArgs rest (M.insert key val keyed) other
parseArgs (val:rest) keyed other = parseArgs rest keyed (other ++ [val])

