module Main where

import qualified Data.Text as T
import           Timeline

main :: IO ()
main = either renderError print . parseGraphs . T.pack =<< getContents

renderError :: String -> IO ()
renderError s = putStrLn $ "Error: " ++ s
