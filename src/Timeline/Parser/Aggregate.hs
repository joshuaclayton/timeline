module Timeline.Parser.Aggregate
    ( StatisticalAggregate(..)
    , smaParser
    , semaParser
    , demaParser
    ) where

import Text.Megaparsec
import Text.Megaparsec.Text
import Timeline.Parser.Internal
import Timeline.Types

smaParser :: Parser StatisticalAggregate
smaParser = do
    duration <- try $ space *> string "+sma" *> parens double

    if duration > 0
        then return $ SimpleMovingAverage $ round duration
        else fail "SMA window must be greater than zero"

semaParser :: Parser StatisticalAggregate
semaParser = do
    alpha <- try $ space *> string "+sema" *> parens double
    if inRange 0 1 alpha
        then return $ SingleExponentialMovingAverage alpha
        else fail "SEMA alpha value must be between 0 and 1"

demaParser :: Parser StatisticalAggregate
demaParser = do
    (alpha, beta) <- try $ space *> string "+dema" *> parens pair

    case (inRange 0 1 alpha, inRange 0 1 beta) of
        (True, True) -> return $ DoubleExponentialMovingAverage alpha beta
        (False, _) -> fail "DEMA alpha value must be between 0 and 1"
        (_, False) -> fail "DEMA beta value must be between 0 and 1"
  where
    pair = do
        p1 <- double <* comma
        p2 <- space *> double
        return (p1, p2)

