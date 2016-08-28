module Timeline.Parser.Aggregate
    ( StatisticalAggregate(..)
    , smaParser
    , semaParser
    , demaParser
    , statisticalAggregateName
    , statisticalAggregateToTimeSeries
    ) where

import qualified Data.Maybe as M
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.Text
import           Timeline.Parser.Internal
import           Timeline.Statistics
import           Timeline.Types

data StatisticalAggregate
    = SimpleMovingAverage Int
    | SingleExponentialMovingAverage Double
    | DoubleExponentialMovingAverage Double Double

smaParser :: Parser StatisticalAggregate
smaParser = do
    duration <- try $ space *> string "+sma" *> parens double

    if duration > 0
        then return $ SimpleMovingAverage $ round duration
        else fail "SMA window must be greater than zero"

semaParser :: Parser StatisticalAggregate
semaParser = do
    alpha <- try $ space *> string "+sema" *> parens double
    if inRange 0 1 alpha
        then return $ SingleExponentialMovingAverage alpha
        else fail "SEMA alpha value must be between 0 and 1"

demaParser :: Parser StatisticalAggregate
demaParser = do
    (alpha, beta) <- try $ space *> string "+dema" *> parens pair

    case (inRange 0 1 alpha, inRange 0 1 beta) of
        (True, True) -> return $ DoubleExponentialMovingAverage alpha beta
        (False, _) -> fail "DEMA alpha value must be between 0 and 1"
        (_, False) -> fail "DEMA beta value must be between 0 and 1"
  where
    pair = do
        p1 <- double <* comma
        p2 <- space *> double
        return (p1, p2)

statisticalAggregateName :: StatisticalAggregate -> Text
statisticalAggregateName (SimpleMovingAverage i) = "SMA(" <> T.pack (show i) <> ")"
statisticalAggregateName (SingleExponentialMovingAverage d) = "SEMA(" <> T.pack (show d) <> ")"
statisticalAggregateName (DoubleExponentialMovingAverage d1 d2) = "DEMA(" <> T.pack (show d1) <> ", " <> T.pack (show d2) <> ")"

statisticalAggregateToTimeSeries :: StatisticalAggregate -> TimeSeriesGraph -> TimeSeriesGraph
statisticalAggregateToTimeSeries sa@(SimpleMovingAverage i) g = LineGraph (Just $ graphName g <> statisticalAggregateName sa) $ simpleMovingAverage i 0 (graphValues g)
statisticalAggregateToTimeSeries sa@(SingleExponentialMovingAverage d) g = LineGraph (Just $ graphName g <> statisticalAggregateName sa) $ map (M.fromMaybe 0 . srSmoothedValue) $ srsResults $ singleExponential d (graphValues g)
statisticalAggregateToTimeSeries sa@(DoubleExponentialMovingAverage d1 d2) g = LineGraph (Just $ graphName g <> statisticalAggregateName sa) $ map (M.fromMaybe 0 . srSmoothedValue) $ srsResults $ doubleExponential d1 d2 (graphValues g)
