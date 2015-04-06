module App.Utility (

	format,
	F.Only(..),
	F.print

) where

import           Data.Text                  (append, pack)
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Format           as F

--
-- We need strict text, so wrap format and make it give strict output.
--
format t v = LT.toStrict $ F.format t v
