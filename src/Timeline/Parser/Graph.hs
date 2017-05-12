module Timeline.Parser.Graph
    ( barParser
    , lineParser
    , scatterPlotParser
    , stackedBarParser
    , boxPlotParser
    ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.Text
import           Timeline.Parser.Internal
import           Timeline.Types

barParser :: Parser TimeSeriesGraph
barParser = BarGraph <$> (chartTypeIntroduction "bar" *> commaDelimitedDecimals)

lineParser :: Parser TimeSeriesGraph
lineParser = LineGraph <$> (chartTypeIntroduction "line" *> commaDelimitedDecimals)

scatterPlotParser :: Parser TimeSeriesGraph
scatterPlotParser = ScatterPlotGraph <$> (chartTypeIntroduction "scatter-plot" *> commaDelimitedThreeTuples)

boxPlotParser :: Parser TimeSeriesGraph
boxPlotParser = do
    lists <- chartTypeIntroduction "box-plot" *> commaDelimitedLists
    if all validListLength lists
        then return $ BoxGraph (buildBoxAndWhisker lists)
        else fail "Box plot members do not contain the correct number of elements"
  where
    validListLength xs = null xs || length xs == 5

buildBoxAndWhisker :: [[Double]] -> [Maybe BoxAndWhisker]
buildBoxAndWhisker = map go
  where
    go (low:q1:median:q3:high:_) = Just $ BoxAndWhisker low q1 median q3 high
    go _ = Nothing

stackedBarParser :: Parser TimeSeriesGraph
stackedBarParser = do
    parsedLists <- chartTypeIntroduction "stacked-bar" *> commaDelimitedLists

    if differentListLengths length parsedLists
        then fail "Stacked bar items do not have equal lengths"
        else return $ StackedBarGraph parsedLists

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

