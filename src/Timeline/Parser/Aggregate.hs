module Timeline.Parser.Aggregate
    ( smaParser
    , semaParser
    , demaParser
    ) where

import           Data.Monoid ((<>))
import qualified Data.MovingAverage as MA
import qualified Data.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.Text
import           Timeline.Parser.Internal
import           Timeline.Types

smaParser :: Graph -> Parser Graph
smaParser g = do
    i <- try $ space *> string "+sma" *> parens int
    build g $ MA.simple i (graphValues g)

semaParser :: Graph -> Parser Graph
semaParser g = do
    alpha <- try $ space *> string "+sema" *> parens double
    build g $ MA.singleExponential alpha (graphValues g)

demaParser :: Graph -> Parser Graph
demaParser g = do
    (alpha, beta) <- try $ space *> string "+dema" *> parens pair
    build g $ MA.doubleExponential alpha beta (graphValues g)
  where
    pair = do
        p1 <- double <* comma
        p2 <- space *> double
        return (p1, p2)

build :: Graph -> Either MA.MovingAverageError (MA.SmoothedResults Double) -> Parser Graph
build g =
    either failWithError buildGraph
  where
    failWithError = fail . show
    buildGraph res = return $ Graph (gname res) $ LineGraph $ resultValues res
    gname res = Just $ graphName g <> T.pack (show $ MA.srsGraphType res)
    resultValues = map MA.srSmoothedValue . MA.srsResults
