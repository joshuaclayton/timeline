module Main where

import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as E
import Timeline

main :: IO ()
main = either renderError renderJSON . parseGraphs . T.pack =<< getContents
  where
    renderJSON = putStrLn . TL.unpack . E.decodeUtf8 . A.encode

renderError :: String -> IO ()
renderError s = putStrLn $ "Error: " ++ s
