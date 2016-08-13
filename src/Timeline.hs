{-# LANGUAGE OverloadedStrings #-}

module Timeline where

import           Control.Applicative ((<|>))
import           Data.Attoparsec.Text
import           Data.Text (Text)
import qualified Data.Text as T

data Graphs = Graphs
    { gGraphList :: [TimeSeriesGraph]
    } deriving Show

data TimeSeriesGraph
    = BarGraph [Double]
    | LineGraph [Double]
    | StackedBar [[Double]]
    deriving Show

parseGraphs :: String -> Either String Graphs
parseGraphs = parseOnly (graphsParser <* endOfInput) . T.strip . T.pack

graphLength :: TimeSeriesGraph -> Int
graphLength (BarGraph a) = length a
graphLength (LineGraph a) = length a
graphLength (StackedBar a) = length a

graphsParser :: Parser Graphs
graphsParser = do
    graphResults <- graphParser `sepBy` endOfLine

    let firstLength = graphLength $ head graphResults

    if all (== firstLength) $ map graphLength graphResults
        then return $ Graphs graphResults
        else mempty <?> "All graphs did not have equal lengths"

graphParser :: Parser TimeSeriesGraph
graphParser =
    barParser
    <|> lineParser
    <|> stackedBarParser

barParser :: Parser TimeSeriesGraph
barParser =
    BarGraph <$> (chartTypeIntroduction "bar" *> commaDelimitedDecimals)

lineParser :: Parser TimeSeriesGraph
lineParser =
    LineGraph <$> (chartTypeIntroduction "line" *> commaDelimitedDecimals)

stackedBarParser :: Parser TimeSeriesGraph
stackedBarParser = do
    parsedLists <- chartTypeIntroduction "stacked-bar" *> commaDelimitedLists

    let firstLength = length $ head parsedLists

    if all (== firstLength) $ map length parsedLists
        then return $ StackedBar parsedLists
        else mempty <?> "Stacked bar items did not have equal lengths"

commaDelimitedDecimals :: Parser [Double]
commaDelimitedDecimals = (skipSpace *> double) `sepBy` char ','

commaDelimitedLists :: Parser [[Double]]
commaDelimitedLists = (skipSpace *> "[" *> commaDelimitedDecimals <* "]") `sepBy` char ','

chartTypeIntroduction :: Text -> Parser ()
chartTypeIntroduction t = string (T.snoc t ':') *> skipSpace
