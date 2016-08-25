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
    deriving (Eq, Show)

graphLength :: TimeSeriesGraph -> Int
graphLength (BarGraph _ a) = length a
graphLength (LineGraph _ a) = length a
graphLength (StackedBarGraph _ a) = length $ head a

graphValues :: TimeSeriesGraph -> [Double]
graphValues (BarGraph _ a) = a
graphValues (LineGraph _ a) = a
graphValues (StackedBarGraph _ as) = foldl1 (zipWith (+)) as
