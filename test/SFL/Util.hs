module SFL.Util where

import           Control.Monad
import           Control.Monad.State.Lazy
import           Data.Either
import qualified Data.Text                as Text
import           Debug.Trace
import           SFL.Lexer
import           SFL.Parser
import           SFL.Type
import           Text.Megaparsec

parse' :: SFLP s a -> SflParseState s -> String -> Either String a
parse' p init arg =
  let res = flip evalState init . runSflParser $ runParserT (p <* eof) "" arg
  in case res of
    Right r -> Right r
    Left e  -> Left $ trace (errorBundlePretty e) $ errorBundlePretty e
