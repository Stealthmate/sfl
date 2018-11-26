module TestUtil where

import           Control.Monad.State.Lazy
import           Debug.Trace
import           SFL.Type
import           Text.Megaparsec

parse' :: SFLP s a -> SflParseState s -> String -> Either String a
parse' p init arg =
  let res = flip evalState init . runSflParser $ runParserT (p <* eof) "" arg
  in case res of
    Right r -> Right r
    Left e  -> Left $ trace (errorBundlePretty e) $ errorBundlePretty e

litd = LiteralE . NumberL
litn = LiteralE . NumberL . fromIntegral
lits = LiteralE . StringL

