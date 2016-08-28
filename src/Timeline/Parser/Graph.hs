module Timeline.Parser.Graph
    ( barParser
    , lineParser
    , scatterPlotParser
    , stackedBarParser
    ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.Text
import           Timeline.Parser.Internal
import           Timeline.Types

barParser :: Maybe Text -> Parser TimeSeriesGraph
barParser mname = BarGraph mname <$> (chartTypeIntroduction "bar" *> commaDelimitedDecimals)

lineParser :: Maybe Text -> Parser TimeSeriesGraph
lineParser mname = LineGraph mname <$> (chartTypeIntroduction "line" *> commaDelimitedDecimals)

scatterPlotParser :: Maybe Text -> Parser TimeSeriesGraph
scatterPlotParser mname = ScatterPlotGraph mname <$> (chartTypeIntroduction "scatter-plot" *> commaDelimitedThreeTuples)

stackedBarParser :: Maybe Text -> Parser TimeSeriesGraph
stackedBarParser mname = do
    parsedLists <- chartTypeIntroduction "stacked-bar" *> commaDelimitedLists

    if differentListLengths length parsedLists
        then fail "Stacked bar items do not have equal lengths"
        else return $ StackedBarGraph mname parsedLists

chartTypeIntroduction :: Text -> Parser ()
chartTypeIntroduction t = string (T.unpack $ T.snoc t ':') *> space

commaDelimitedDecimals :: Parser [Double]
commaDelimitedDecimals = (space *> double) `sepBy` comma

commaDelimitedLists :: Parser [[Double]]
commaDelimitedLists = (space *> brackets commaDelimitedDecimals) `sepBy` comma

commaDelimitedThreeTuples :: Parser [(Text, Double, Double)]
commaDelimitedThreeTuples = (space *> parens innerTuple) `sepBy` comma
  where
    innerTuple = do
      value <- T.pack <$> manyTill anyChar comma
      p1 <- space *> double <* comma
      p2 <- space *> double
      return (value, p1, p2)
