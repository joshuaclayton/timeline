{-# OPTIONS_GHC -fno-warn-orphans #-}

module Timeline.Server.Json where

import Data.Aeson
import Timeline

instance ToJSON Graphs where
    toJSON (Graphs gs) =
        object [ "graphs" .= map toJSON gs ]

instance ToJSON TimeSeriesGraph where
    toJSON (LineGraph is) = object [ "_type" .= String "line", "points" .= map toJSON is ]
    toJSON (BarGraph is) = object [ "_type" .= String "bar", "points" .= map toJSON is ]
    toJSON (StackedBar is) = object [ "_type" .= String "stacked-bar", "points" .= map toJSON is ]
