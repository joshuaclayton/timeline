module Timeline.Parser
    ( parseGraphs
    ) where

import           Control.Applicative ((<|>))
import           Data.Attoparsec.Text
import qualified Data.Maybe as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Timeline.Statistics
import           Timeline.Types

data StatisticalAggregate
    = SimpleMovingAverage Int
    | SingleExponentialMovingAverage Double
    | DoubleExponentialMovingAverage Double Double

parseGraphs :: Text -> Either String Graphs
parseGraphs = parseOnly (graphsParser <* endOfInput) . T.strip

processResults :: (TimeSeriesGraph, [StatisticalAggregate]) -> [TimeSeriesGraph]
processResults (g, sas) = g : map (`processGraph` graphValues g) sas

processGraph :: StatisticalAggregate -> [Double] -> TimeSeriesGraph
processGraph (SimpleMovingAverage i) = LineGraph . simpleMovingAverage i 0
processGraph (SingleExponentialMovingAverage d) = LineGraph . map (M.fromMaybe 0 . srSmoothedValue) . srsResults . singleExponential d
processGraph (DoubleExponentialMovingAverage d1 d2) = LineGraph . map (M.fromMaybe 0 . srSmoothedValue) . srsResults . doubleExponential d1 d2

graphsParser :: Parser Graphs
graphsParser = do
    graphResults <- concatMap processResults <$> graphParser `sepBy1` endOfLine

    case (differentListLengths graphLength graphResults, missingPoints graphLength graphResults) of
        (True, _) -> fail "Not all graphs had the same length"
        (_, True) -> fail "No points were provided"
        (_, _) -> return $ Graphs graphResults
  where
    missingPoints f = elem 0 . map f

smaParser :: Parser StatisticalAggregate
smaParser = do
    duration <- skipSpace *> "+sma(" *> double <* ")"
    if duration >= 0
        then return $ SimpleMovingAverage $ round duration
        else fail "SMA window is less than zero"

semaParser :: Parser StatisticalAggregate
semaParser = do
    duration <- skipSpace *> "+sema(" *> double <* ")"
    if inRange 0 1 duration
        then return $ SingleExponentialMovingAverage duration
        else fail "SEMA alpha value is not between 0 and 1"

demaParser :: Parser StatisticalAggregate
demaParser = do
    (p1, p2) <- skipSpace *> "+dema(" *> pair <* ")"
    return $ DoubleExponentialMovingAverage p1 p2
  where
    pair = do
        p1 <- double <* ","
        p2 <- skipSpace *> double
        case (inRange 0 1 p1, inRange 0 1 p2) of
            (True, True) -> return (p1, p2)
            (False, _) -> fail "DEMA alpha value is not between 0 and 1"
            (_, False) -> fail "DEMA beta value is not between 0 and 1"

inRange :: (Ord a, Num a) => a -> a -> a -> Bool
inRange min' max' value = value >= min' && value <= max'

additionalLines :: Parser StatisticalAggregate
additionalLines = smaParser <|> semaParser <|> demaParser

graphParser :: Parser (TimeSeriesGraph, [StatisticalAggregate])
graphParser = do
    initial <- barParser <|> lineParser <|> stackedBarParser
    additional <- many' additionalLines

    return (initial, additional)

barParser :: Parser TimeSeriesGraph
barParser = BarGraph <$> (chartTypeIntroduction "bar" *> commaDelimitedDecimals)

lineParser :: Parser TimeSeriesGraph
lineParser = LineGraph <$> (chartTypeIntroduction "line" *> commaDelimitedDecimals)

stackedBarParser :: Parser TimeSeriesGraph
stackedBarParser = do
    parsedLists <- chartTypeIntroduction "stacked-bar" *> commaDelimitedLists

    if differentListLengths length parsedLists
        then fail "Stacked bar items did not have equal lengths"
        else return $ StackedBar parsedLists

commaDelimitedDecimals :: Parser [Double]
commaDelimitedDecimals = (skipSpace *> double) `sepBy` char ','

commaDelimitedLists :: Parser [[Double]]
commaDelimitedLists = (skipSpace *> "[" *> commaDelimitedDecimals <* "]") `sepBy` char ','

chartTypeIntroduction :: Text -> Parser ()
chartTypeIntroduction t = string (T.snoc t ':') *> skipSpace

differentListLengths :: (x -> Int) -> [x] -> Bool
differentListLengths f xs = any (/= (f $ head xs)) $ map f xs
