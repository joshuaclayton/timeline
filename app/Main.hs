module Main where

import Timeline

main :: IO ()
main = either renderError print . parseGraphs =<< getContents

renderError :: String -> IO ()
renderError s = putStrLn $ "Error: " ++ s
