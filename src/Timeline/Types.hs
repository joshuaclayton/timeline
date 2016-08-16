module Timeline.Types
    ( Graphs(..)
    , TimeSeriesGraph(..)
    , graphLength
    ) where

data Graphs = Graphs [TimeSeriesGraph] deriving (Eq, Show)

data TimeSeriesGraph
    = BarGraph [Double]
    | LineGraph [Double]
    | StackedBar [[Double]]
    deriving (Eq, Show)

graphLength :: TimeSeriesGraph -> Int
graphLength (BarGraph a) = length a
graphLength (LineGraph a) = length a
graphLength (StackedBar a) = length $ head a
