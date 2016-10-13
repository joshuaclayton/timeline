{-# LANGUAGE FlexibleInstances #-}

module Timeline.Types
    ( Graphs(..)
    , TimeSeriesGraph(..)
    , BoxAndWhisker(..)
    , graphLength
    , graphValues
    , graphName
    ) where

import           Data.Aeson
import qualified Data.Maybe as M
import           Data.Text (Text)
import qualified Data.Text as T

data Graphs = Graphs [TimeSeriesGraph] deriving (Eq, Show)

data TimeSeriesGraph
    = BarGraph (Maybe Text) [Double]
    | LineGraph (Maybe Text) [Double]
    | StackedBarGraph (Maybe Text) [[Double]]
    | ScatterPlotGraph (Maybe Text) [(Text, Double, Double)]
    | BoxGraph (Maybe Text) [Maybe BoxAndWhisker]
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

instance ToJSON TimeSeriesGraph where
    toJSON (LineGraph n is) = object [ "_type" .= String "line", "name" .= n, "points" .= map toJSON is ]
    toJSON (BarGraph n is) = object [ "_type" .= String "bar", "name" .= n, "points" .= map toJSON is ]
    toJSON (StackedBarGraph n is) = object [ "_type" .= String "stacked-bar", "name" .= n, "points" .= map toJSON is ]
    toJSON (ScatterPlotGraph n is) = object [ "_type" .= String "scatter-plot", "name" .= n, "points" .= map toJSON is ]
    toJSON (BoxGraph n is) = object [ "_type" .= String "box-plot", "name" .= n, "points" .= map toJSON is ]

instance {-# OVERLAPPING #-} ToJSON (Maybe BoxAndWhisker) where
    toJSON = toJSON . extract
      where
        extract (Just (BoxAndWhisker low q1 median q3 high)) = [low, q1, median, q3, high]
        extract Nothing = []

graphLength :: TimeSeriesGraph -> Int
graphLength (BarGraph _ a) = length a
graphLength (LineGraph _ a) = length a
graphLength (StackedBarGraph _ a) = length $ head a
graphLength (ScatterPlotGraph _ a) = length a
graphLength (BoxGraph _ a) = length a

graphValues :: TimeSeriesGraph -> [Double]
graphValues (BarGraph _ a) = a
graphValues (LineGraph _ a) = a
graphValues (StackedBarGraph _ as) = foldl1 (zipWith (+)) as
graphValues (ScatterPlotGraph _ as) = map (\(_, _, y) -> y) as
graphValues (BoxGraph _ as) = map (maybe 0 bawMedian) as

graphName :: TimeSeriesGraph -> Text
graphName (BarGraph mname _) = M.fromMaybe "" $ (`T.append` ": ") <$> mname
graphName (LineGraph mname _) = M.fromMaybe "" $ (`T.append` ": ") <$> mname
graphName (StackedBarGraph mname _) = M.fromMaybe "" $ (`T.append` ": ") <$> mname
graphName (ScatterPlotGraph mname _) = M.fromMaybe "" $ (`T.append` ": ") <$> mname
graphName (BoxGraph mname _) = M.fromMaybe "" $ (`T.append` ": ") <$> mname
