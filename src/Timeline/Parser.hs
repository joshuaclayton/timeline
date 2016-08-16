module Timeline.Parser
    ( parseGraphs
    ) where

import           Control.Applicative ((<|>))
import           Data.Attoparsec.Text
import           Data.Text (Text)
import qualified Data.Text as T
import           Timeline.Types

parseGraphs :: Text -> Either String Graphs
parseGraphs = parseOnly (graphsParser <* endOfInput) . T.strip

graphsParser :: Parser Graphs
graphsParser = do
    graphResults <- graphParser `sepBy1` endOfLine

    case (differentListLengths graphLength graphResults, missingPoints graphLength graphResults) of
        (True, _) -> fail "Not all graphs had the same length"
        (_, True) -> fail "No points were provided"
        (_, _) -> return $ Graphs graphResults
  where
    missingPoints f = elem 0 . map f

graphParser :: Parser TimeSeriesGraph
graphParser = barParser <|> lineParser <|> stackedBarParser

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
