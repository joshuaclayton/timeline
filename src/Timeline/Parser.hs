module Timeline.Parser
    ( parseGraphs
    ) where

import qualified Data.Maybe as M
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

processResults :: (TimeSeriesGraph, [StatisticalAggregate]) -> [TimeSeriesGraph]
processResults (g, sas) = g : map (`processGraph` graphValues g) sas

graphsParser :: Parser Graphs
graphsParser = do
    graphResults <- concatMap processResults <$> graphParser `sepBy1` newline

    case (differentListLengths graphLength graphResults, missingPoints graphLength graphResults) of
        (True, _) -> fail "Not all graphs had the same length"
        (_, True) -> fail "No points were provided"
        (_, _) -> return $ Graphs graphResults
  where
    missingPoints f = elem 0 . map f

graphParser :: Parser (TimeSeriesGraph, [StatisticalAggregate])
graphParser = do
    initial <- barParser <|> lineParser <|> stackedBarParser
    additional <- many $ smaParser <|> semaParser <|> demaParser

    return (initial, additional)

barParser :: Parser TimeSeriesGraph
barParser = BarGraph <$> (chartTypeIntroduction "bar" *> commaDelimitedDecimals)

lineParser :: Parser TimeSeriesGraph
lineParser = LineGraph <$> (chartTypeIntroduction "line" *> commaDelimitedDecimals)

stackedBarParser :: Parser TimeSeriesGraph
stackedBarParser = do
    parsedLists <- chartTypeIntroduction "stacked-bar" *> commaDelimitedLists

    if differentListLengths length parsedLists
        then fail "Stacked bar items do not have equal lengths"
        else return $ StackedBarGraph parsedLists

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

chartTypeIntroduction :: Text -> Parser ()
chartTypeIntroduction t = string (T.unpack $ T.snoc t ':') *> space

processGraph :: StatisticalAggregate -> [Double] -> TimeSeriesGraph
processGraph (SimpleMovingAverage i) = LineGraph . simpleMovingAverage i 0
processGraph (SingleExponentialMovingAverage d) = LineGraph . map (M.fromMaybe 0 . srSmoothedValue) . srsResults . singleExponential d
processGraph (DoubleExponentialMovingAverage d1 d2) = LineGraph . map (M.fromMaybe 0 . srSmoothedValue) . srsResults . doubleExponential d1 d2

inRange :: (Ord a, Num a) => a -> a -> a -> Bool
inRange min' max' value = value >= min' && value <= max'

differentListLengths :: (x -> Int) -> [x] -> Bool
differentListLengths f xs = any (/= (f $ head xs)) $ map f xs
