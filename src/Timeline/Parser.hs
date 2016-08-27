module Timeline.Parser
    where

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

parseGraphs :: Text -> Either String Graphs
parseGraphs = parseOnly (graphsParser <* eof) . T.strip

graphsParser :: Parser Graphs
graphsParser = do
    graphResults <- concatMap processResults <$> graphParser `sepBy1` newline

    let badListLengths = differentListLengths graphLength graphResults
        noPoints = missingPoints graphLength graphResults

    case (badListLengths, noPoints) of
        (True, _) -> fail "Not all graphs had the same length"
        (_, True) -> fail "No points were provided"
        (_, _) -> return $ Graphs graphResults
  where
    missingPoints f = elem 0 . map f

processResults :: (TimeSeriesGraph, [StatisticalAggregate]) -> [TimeSeriesGraph]
processResults (g, sas) = g : map (`processGraph` g) sas

graphParser :: Parser (TimeSeriesGraph, [StatisticalAggregate])
graphParser = do
    mname <- nameParser
    initial <- barParser mname <|> lineParser mname <|> stackedBarParser mname <|> scatterPlotParser mname
    additional <- many $ smaParser <|> semaParser <|> demaParser

    return (initial, additional)

nameParser :: Parser (Maybe Text)
nameParser = fmap T.pack <$> optional (char '"' *> manyTill anyChar (char '"') <* char ':') <* space

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
        p1 <- double <* string ","
        p2 <- space *> double
        return (p1, p2)

commaDelimitedDecimals :: Parser [Double]
commaDelimitedDecimals = (space *> double) `sepBy` char ','

commaDelimitedLists :: Parser [[Double]]
commaDelimitedLists = (space *> brackets commaDelimitedDecimals) `sepBy` char ','

commaDelimitedThreeTuples :: Parser [(Text, Double, Double)]
commaDelimitedThreeTuples = (space *> parens innerTuple) `sepBy` char ','
  where
    innerTuple = do
      value <- T.pack <$> manyTill anyChar (char ',')
      p1 <- space *> double <* string ","
      p2 <- space *> double
      return (value, p1, p2)

chartTypeIntroduction :: Text -> Parser ()
chartTypeIntroduction t = string (T.unpack $ T.snoc t ':') *> space

processGraph :: StatisticalAggregate -> TimeSeriesGraph -> TimeSeriesGraph
processGraph sa@(SimpleMovingAverage i) g = LineGraph (Just $ graphName g <> statisticalAggregateName sa) $ simpleMovingAverage i 0 (graphValues g)
processGraph sa@(SingleExponentialMovingAverage d) g = LineGraph (Just $ graphName g <> statisticalAggregateName sa) $ map (M.fromMaybe 0 . srSmoothedValue) $ srsResults $ singleExponential d (graphValues g)
processGraph sa@(DoubleExponentialMovingAverage d1 d2) g = LineGraph (Just $ graphName g <> statisticalAggregateName sa) $ map (M.fromMaybe 0 . srSmoothedValue) $ srsResults $ doubleExponential d1 d2 (graphValues g)

graphName :: TimeSeriesGraph -> Text
graphName (BarGraph mname _) = M.fromMaybe "" $ (`T.append` ": ") <$> mname
graphName (LineGraph mname _) = M.fromMaybe "" $ (`T.append` ": ") <$> mname
graphName (StackedBarGraph mname _) = M.fromMaybe "" $ (`T.append` ": ") <$> mname
graphName (ScatterPlotGraph mname _) = M.fromMaybe "" $ (`T.append` ": ") <$> mname

statisticalAggregateName :: StatisticalAggregate -> Text
statisticalAggregateName (SimpleMovingAverage i) = "SMA(" <> T.pack (show i) <> ")"
statisticalAggregateName (SingleExponentialMovingAverage d) = "SEMA(" <> T.pack (show d) <> ")"
statisticalAggregateName (DoubleExponentialMovingAverage d1 d2) = "DEMA(" <> T.pack (show d1) <> ", " <> T.pack (show d2) <> ")"

inRange :: (Ord a, Num a) => a -> a -> a -> Bool
inRange min' max' value = value >= min' && value <= max'

differentListLengths :: (x -> Int) -> [x] -> Bool
differentListLengths f xs = any (/= (f $ head xs)) $ map f xs
