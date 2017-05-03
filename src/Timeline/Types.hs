{-# LANGUAGE FlexibleInstances #-}

module Timeline.Types
    ( Graphs(..)
    , Graph(..)
    , TimeSeriesGraph(..)
    , BoxAndWhisker(..)
    , StatisticalAggregate(..)
    , graphLength
    , graphValues
    , graphName
    , statisticalAggregateToTimeSeries
    ) where

import           Data.Aeson
import qualified Data.Maybe as M
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Timeline.Statistics

data Graphs = Graphs [Graph] deriving (Eq, Show)

data Graph = Graph (Maybe Text) TimeSeriesGraph deriving (Eq, Show)

data TimeSeriesGraph
    = BarGraph [Double]
    | LineGraph [Double]
    | StackedBarGraph [[Double]]
    | ScatterPlotGraph [(Text, Double, Double)]
    | BoxGraph [Maybe BoxAndWhisker]
    deriving (Eq, Show)

data StatisticalAggregate
    = SimpleMovingAverage Int
    | SingleExponentialMovingAverage Double
    | DoubleExponentialMovingAverage Double Double

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

statisticalAggregateName :: StatisticalAggregate -> Text
statisticalAggregateName (SimpleMovingAverage i) = "SMA(" <> T.pack (show i) <> ")"
statisticalAggregateName (SingleExponentialMovingAverage d) = "SEMA(" <> T.pack (show d) <> ")"
statisticalAggregateName (DoubleExponentialMovingAverage d1 d2) = "DEMA(" <> T.pack (show d1) <> ", " <> T.pack (show d2) <> ")"

aggToSeries :: StatisticalAggregate -> Graph -> [Double]
aggToSeries (SimpleMovingAverage i) g = simpleMovingAverage i 0 (graphValues g)
aggToSeries (SingleExponentialMovingAverage d) g = map (M.fromMaybe 0 . srSmoothedValue) $ srsResults $ singleExponential d (graphValues g)
aggToSeries (DoubleExponentialMovingAverage d1 d2) g = map (M.fromMaybe 0 . srSmoothedValue) $ srsResults $ doubleExponential d1 d2 (graphValues g)

statisticalAggregateToTimeSeries :: StatisticalAggregate -> Graph -> Graph
statisticalAggregateToTimeSeries sa g = Graph gname graph
  where
    gname = Just $ graphName g <> statisticalAggregateName sa
    graph = LineGraph $ aggToSeries sa g
