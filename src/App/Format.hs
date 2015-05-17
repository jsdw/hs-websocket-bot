module App.Format (

    module Data.Text.Format,
    printLn,
    formatLn,

) where

import           Prelude                  hiding (print)
import           Data.Text.Format
import           Data.Text.Format.Params
import           Data.Text.Buildable      as B
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BL
import           Data.Text.Encoding       (decodeUtf8)
import           Data.Monoid              ((<>))
import           Control.Monad.Trans      (MonadIO(..))


--make instances for bytestring as well to be printed, assuming UTF8 encoded.
--efficiency not important, just making it easier to print and inspect output.

instance Buildable BS.ByteString where
    build = B.build . decodeUtf8

instance Buildable BL.ByteString where
    build = B.build . decodeUtf8 . BL.toStrict

printLn msg b = print (msg<>"\n") b
formatLn msg b = format (msg<>"\n") b