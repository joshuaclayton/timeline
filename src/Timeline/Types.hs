module Timeline.Types
    ( Graphs(..)
    , TimeSeriesGraph(..)
    , graphLength
    , graphValues
    ) where

import Data.Text (Text)

data Graphs = Graphs [TimeSeriesGraph] deriving (Eq, Show)

data TimeSeriesGraph
    = BarGraph (Maybe Text) [Double]
    | LineGraph (Maybe Text) [Double]
    | StackedBarGraph (Maybe Text) [[Double]]
    | ScatterPlotGraph (Maybe Text) [(Text, Double, Double)]
    deriving (Eq, Show)

graphLength :: TimeSeriesGraph -> Int
graphLength (BarGraph _ a) = length a
graphLength (LineGraph _ a) = length a
graphLength (StackedBarGraph _ a) = length $ head a
graphLength (ScatterPlotGraph _ a) = length a

graphValues :: TimeSeriesGraph -> [Double]
graphValues (BarGraph _ a) = a
graphValues (LineGraph _ a) = a
graphValues (StackedBarGraph _ as) = foldl1 (zipWith (+)) as
graphValues (ScatterPlotGraph _ as) = map (\(_, _, y) -> y) as
