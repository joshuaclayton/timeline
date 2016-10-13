module Timeline.Parser
    ( parseGraphs
    ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec
import           Text.Megaparsec.Text
import           Timeline.Parser.Aggregate
import           Timeline.Parser.Graph
import           Timeline.Parser.Internal
import           Timeline.Types

parseGraphs :: Text -> Either String Graphs
parseGraphs = parseOnly (graphsParser <* eof) . T.strip

graphsParser :: Parser Graphs
graphsParser = do
    graphResults <- concatMap processResults <$> graphParser `sepBy1` newline

    let badListLengths = differentListLengths graphLength graphResults
        noPoints = missingPoints graphLength graphResults

    case (badListLengths, noPoints) of
        (True, _) -> fail $ "Not all graphs had the same length: " ++ show (map graphLength graphResults)
        (_, True) -> fail "No points were provided"
        (_, _) -> return $ Graphs graphResults
  where
    missingPoints f = elem 0 . map f

processResults :: (TimeSeriesGraph, [StatisticalAggregate]) -> [TimeSeriesGraph]
processResults (g, sas) = g : map (`statisticalAggregateToTimeSeries` g) sas

graphParser :: Parser (TimeSeriesGraph, [StatisticalAggregate])
graphParser = do
    mname <- nameParser
    initial <- barParser mname <|> lineParser mname <|> stackedBarParser mname <|> scatterPlotParser mname <|> boxPlotParser mname
    additional <- many $ smaParser <|> semaParser <|> demaParser

    return (initial, additional)

nameParser :: Parser (Maybe Text)
nameParser = fmap T.pack <$> optional (char '"' *> manyTill anyChar (char '"') <* char ':') <* space
