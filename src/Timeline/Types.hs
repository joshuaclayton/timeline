{-# LANGUAGE FlexibleInstances #-}

module Timeline.Types
    ( Graphs(..)
    , Graph(..)
    , TimeSeriesGraph(..)
    , BoxAndWhisker(..)
    , graphLength
    , graphValues
    , graphName
    ) where

import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T

data Graphs = Graphs [Graph] deriving (Eq, Show)

data Graph = Graph (Maybe Text) TimeSeriesGraph deriving (Eq, Show)

data TimeSeriesGraph
    = BarGraph [Double]
    | LineGraph [Double]
    | StackedBarGraph [[Double]]
    | ScatterPlotGraph [(Text, Double, Double)]
    | BoxGraph [Maybe BoxAndWhisker]
    deriving (Eq, Show)

data BoxAndWhisker = BoxAndWhisker
    { bawLow :: Double
    , bawQ1 :: Double
    , bawMedian :: Double
    , bawQ3 :: Double
    , bawHigh :: Double
    } deriving (Eq, Show)

instance ToJSON Graphs where
    toJSON (Graphs gs) =
        object [ "graphs" .= map toJSON gs ]

instance ToJSON Graph where
    toJSON (Graph n g) =
        object [ "_type" .= String (graphType g), "name" .= n, "points" .= map toJSON (graphValues' g) ]

instance {-# OVERLAPPING #-} ToJSON (Maybe BoxAndWhisker) where
    toJSON = toJSON . extract
      where
        extract (Just (BoxAndWhisker low q1 median q3 high)) = [low, q1, median, q3, high]
        extract Nothing = []

graphLength :: Graph -> Int
graphLength (Graph _ g) = graphLength' g

graphLength' :: TimeSeriesGraph -> Int
graphLength' (BarGraph a) = length a
graphLength' (LineGraph a) = length a
graphLength' (StackedBarGraph a) = length $ head a
graphLength' (ScatterPlotGraph a) = length a
graphLength' (BoxGraph a) = length a

graphValues :: Graph -> [Double]
graphValues (Graph _ g) = graphValues' g

graphValues' :: TimeSeriesGraph -> [Double]
graphValues' (BarGraph a) = a
graphValues' (LineGraph a) = a
graphValues' (StackedBarGraph as) = foldl1 (zipWith (+)) as
graphValues' (ScatterPlotGraph as) = map (\(_, _, y) -> y) as
graphValues' (BoxGraph as) = map (maybe 0 bawMedian) as

graphName :: Graph -> Text
graphName = maybe "" (`T.append` ": ") . graphName'

graphName' :: Graph -> Maybe Text
graphName' (Graph mname _) = mname

graphType :: TimeSeriesGraph -> Text
graphType (BarGraph _) = "bar"
graphType (LineGraph _) = "line"
graphType (StackedBarGraph _) = "stacked-bar"
graphType (ScatterPlotGraph _) = "scatter-plot"
graphType (BoxGraph _) = "box-plot"
