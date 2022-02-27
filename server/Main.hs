module Main where

import qualified Data.Char as C
import qualified Data.Maybe as M
import           Options.Applicative
import           Timeline.Server.Server

data Options = Options
    { oPort :: Maybe Int
    }

main :: IO ()
main = runProgram =<< parseCLI

defaultPort :: Int
defaultPort = 5515

parseCLI :: IO Options
parseCLI =
    execParser (withInfo parseOptions pHeader pDescription pFooter)
  where
    pHeader      = "Timeline Server"
    pDescription = "An HTTP JSON API for parsing Timeline-formatted input"
    pFooter      = "CLI USAGE: $ timeline-server"

runProgram :: Options -> IO ()
runProgram = runApp . M.fromMaybe defaultPort . oPort

parseOptions :: Parser Options
parseOptions = Options <$> parsePort

parsePort :: Parser (Maybe Int)
parsePort = (stringToInt =<<) <$> portParser
  where
    portParser = optional $ strOption $
        short 'p'
        <> long "port"
        <> help "Port to run the server at"

withInfo :: Parser a -> String -> String -> String -> ParserInfo a
withInfo opts h d f =
    info (helper <*> opts) $ header h <> progDesc d <> footer f

stringToInt :: String -> Maybe Int
stringToInt xs
    | all C.isDigit xs = Just $ loop 0 xs
    | otherwise = Nothing
  where
    loop = foldl (\acc x -> acc * 10 + C.digitToInt x)
