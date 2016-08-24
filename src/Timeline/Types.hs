module Timeline.Types
    ( Graphs(..)
    , TimeSeriesGraph(..)
    , graphLength
    , graphValues
    ) where

data Graphs = Graphs [TimeSeriesGraph] deriving (Eq, Show)

data TimeSeriesGraph
    = BarGraph [Double]
    | LineGraph [Double]
    | StackedBarGraph [[Double]]
    deriving (Eq, Show)

graphLength :: TimeSeriesGraph -> Int
graphLength (BarGraph a) = length a
graphLength (LineGraph a) = length a
graphLength (StackedBarGraph a) = length $ head a

graphValues :: TimeSeriesGraph -> [Double]
graphValues (BarGraph a) = a
graphValues (LineGraph a) = a
graphValues (StackedBarGraph as) = foldl1 (zipWith (+)) as
