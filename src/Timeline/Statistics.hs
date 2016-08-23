module Timeline.Statistics
    ( simpleMovingAverage
    , singleExponential
    , doubleExponential
    , SmoothedResults(..)
    , SmoothedResult(..)
    ) where

import qualified Data.Bifunctor as BF
import qualified Data.Maybe as M

data SmoothedResults a = SmoothedResults
    { srsResults :: [SmoothedResult a]
    , srsSumSquaredErrors :: a
    , srsMeanSquaredErrors :: a
    } deriving Show

data SmoothedResult a = SmoothedResult
    { srValue :: a
    , srSmoothedValue :: Maybe a
    , srError :: Maybe a
    , srErrorSquared :: Maybe a
    } deriving Show

simpleMovingAverage :: (Fractional a) => Int -> a -> [a] -> [a]
simpleMovingAverage _ _ [] = []
simpleMovingAverage n s samples
  | n <= 0 = []
  | otherwise =
      map fst3 $ scanl1 average sample_triples
      where
        divisors = map fromIntegral $ [1..n] ++ repeat n
        n_agos = replicate (n - 1) s ++ samples
        sample_triples = zip3 samples divisors n_agos

average :: (Fractional a) => (a,a,a) -> (a,a,a) -> (a,a,a)
average (prev_avg, prev_div, dropme) (sample, divisor, n_ago) =
  (new_avg, divisor, n_ago)
  where
    prev_sum = prev_avg * prev_div
    new_avg = (prev_sum + sample - dropme) / divisor

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

singleExponential :: Floating a => a -> [a] -> SmoothedResults a
singleExponential a xs = buildSmoothedResults $ zipWith (curry fn) [1..] xs
  where
    fn (idx, v) = SmoothedResult v smoothed smoothedError smoothedErrorSquared
      where
        smoothed = singleExponentialSmoothed idx a $ take idx xs
        smoothedError = fmap (-) (Just v) <*> smoothed
        smoothedErrorSquared = fmap (**) smoothedError <*> Just 2

buildSmoothedResults :: Floating a => [SmoothedResult a] -> SmoothedResults a
buildSmoothedResults xs = SmoothedResults xs sumSquaredErrors meanSquaredErrors
  where
    sumSquaredErrors = sum squaredErrors
    meanSquaredErrors = sumSquaredErrors / fromIntegral (length squaredErrors)
    squaredErrors = M.mapMaybe srErrorSquared xs

singleExponentialSmoothed :: Floating a => Int -> a -> [a] -> Maybe a
singleExponentialSmoothed 1 _ _ = Nothing
singleExponentialSmoothed 2 _ (x:_) = Just x
singleExponentialSmoothed t a xs' =
    fmap (+) currentValue <*> previousSmoothed
  where
    currentValue = Just $ a * (reverse xs' !! 1)
    previousSmoothed = fmap (*) (singleExponentialSmoothed (t - 1) a (init xs')) <*> Just (1 - a)

doubleExponential :: (Show a, Floating a) => a -> a -> [a] -> SmoothedResults a
doubleExponential a b xs = buildSmoothedResults $ M.mapMaybe fst $ scanl fn (Nothing, []) $ zip [1..] xs
  where
    fn state (idx, v) = (Just $ SmoothedResult v smoothed smoothedError smoothedErrorSquared, newState)
      where
        smoothed' = doubleExponentialSmoothed (snd state) idx a b $ take idx xs
        smoothed = fst smoothed'
        newState = snd smoothed'
        smoothedError = fmap (-) (Just v) <*> smoothed
        smoothedErrorSquared = fmap (**) smoothedError <*> Just 2

doubleExponentialSmoothed :: (Floating a, Show a) => [a] -> Int -> a -> a -> [a] -> (Maybe a, [a])
doubleExponentialSmoothed state t a b xs' = BF.first Just $ st t state
  where
    st 1 s = (head xs', head xs' : s)
    st t' s = (oldCalc, s ++ [oldCalc])
      where
        oldCalc = (a * (xs' !! (t' - 1))) + (1 - a) * (s !! (t' - 2) + fst (bt (t' - 1) s))
    bt 1 s = (xs' !! 1 - head xs', s)
    bt t' s = (oldCalc, s)
      where
        oldCalc = b * (s !! (t' - 1) - s !! (t' - 2)) + (1 - b) * fst (bt (t' - 1) s)
