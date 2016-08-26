{-# OPTIONS_GHC -fno-warn-orphans #-}

module Timeline.Server.Json where

import Data.Aeson
import Timeline

instance ToJSON Graphs where
    toJSON (Graphs gs) =
        object [ "graphs" .= map toJSON gs ]

instance ToJSON TimeSeriesGraph where
    toJSON (LineGraph n is) = object [ "_type" .= String "line", "name" .= n, "points" .= map toJSON is ]
    toJSON (BarGraph n is) = object [ "_type" .= String "bar", "name" .= n, "points" .= map toJSON is ]
    toJSON (StackedBarGraph n is) = object [ "_type" .= String "stacked-bar", "name" .= n, "points" .= map toJSON is ]
    toJSON (ScatterPlotGraph n is) = object [ "_type" .= String "scatter-plot", "name" .= n, "points" .= map toJSON is ]
