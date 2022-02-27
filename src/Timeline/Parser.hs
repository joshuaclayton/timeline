module Timeline.Parser
  ( parseGraphs
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Text
import Timeline.Parser.Aggregate
import Timeline.Parser.Graph
import Timeline.Parser.Internal
import Timeline.Types

parseGraphs :: Text -> Either String Graphs
parseGraphs = parseOnly (graphsParser <* eof) . T.strip

graphsParser :: Parser Graphs
graphsParser = do
  graphResults <- concat <$> graphAndAggregatesParser `sepBy1` newline
  let badListLengths = differentListLengths graphLength graphResults
      noPoints = missingPoints graphLength graphResults
  case (badListLengths, noPoints) of
    (True, _) ->
      fail $
      "Not all graphs had the same length: " ++
      show (map graphLength graphResults)
    (_, True) -> fail "No points were provided"
    (_, _) -> return $ Graphs graphResults
  where
    missingPoints f = elem 0 . map f

graphAndAggregatesParser :: Parser [Graph]
graphAndAggregatesParser = do
  initial <- graphParser
  additional <-
    many $ choice [smaParser initial, semaParser initial, demaParser initial]
  return (initial : additional)

graphParser :: Parser Graph
graphParser =
  Graph <$> nameParser <*>
  choice
    [barParser, lineParser, stackedBarParser, scatterPlotParser, boxPlotParser]

nameParser :: Parser (Maybe Text)
nameParser =
  fmap T.pack <$> optional (char '"' *> manyTill anyChar (char '"') <* char ':') <*
  space
