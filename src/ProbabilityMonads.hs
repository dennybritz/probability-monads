module ProbabilityMonads where

import           Control.Applicative
import qualified Data.Map            as M
import           Text.Printf

-- | We use a Double as the representation for probabilities.
type Prob = Double

-- | A distribution is represented as a list of possible values
-- and their probabilities
newtype Dist a = Dist [(a, Prob)]

-- | Helper function to access the inner list wrapped by the distribution
unpackDist :: Dist a -> [(a, Prob)]
unpackDist (Dist xs) = xs

-- | Combines outcomes that occur multiple time
squishD :: (Ord a) => Dist a -> Dist a
squishD (Dist xs) = Dist $ M.toList $ M.fromListWith (+) xs

-- | Sum all probilities in the given list
sumP :: [(a, Prob)] -> Prob
sumP = sum . map snd

-- | Normalize the probabilities to 1.0
normP :: [(a, Prob)] -> [(a, Prob)]
normP xs = [(x, p / q) | let q = sumP xs, (x, p) <- xs]

instance (Show a, Ord a) => Show (Dist a) where
  show d = concatMap showRow $ (unpackDist . squishD) d
    where
      showRow (elem, prob) = padded elem ++ " | " ++ printf "%.4f" prob ++ "\n"
      padded elem = replicate (maxElemLen - (length . show) elem) ' ' ++ show elem
      maxElemLen = maximum $ map (length . show . fst) (unpackDist d)

-- | An event maps an outcome to a truth value
type Event a = a -> Bool

-- | Evaluate the probability for the given event
evalD :: Event a -> Dist a -> Prob
evalD p = sumP . filter (p . fst) . unpackDist

-- Helpers to create distributions
-- ========================================

-- | Create a uniform distribution
uniform :: [a] -> Dist a
uniform xs = Dist . normP $ map (,1.0) xs

-- | A fair n-sided die
die :: Int -> Dist Int
die n = uniform [1 .. n]

-- | A coin that lands on x with probability f and y with probability 1-f
coin :: Prob -> a -> a -> Dist a
coin f x y
  | f < 0.0 || f > 1.0 = error "f must be between 0 and 1"
  | otherwise = Dist [(x, f), (y, 1 - f)]

-- Functor, Applicative, and Monad
-- ========================================

-- We  apply the given function to each value in the distribution
instance Functor Dist where
  fmap f (Dist xs) = Dist $ [(f x, p) | (x, p) <- xs]

instance Applicative Dist where
  -- pure :: a -> Dist a
  pure x = Dist [(x, 1.0)]

  -- (<*>) :: Dist (a -> b) -> Dist a -> Dist b
  (Dist fs) <*> (Dist xs) = Dist $ do
    (x, px) <- xs
    (f, pf) <- fs
    return (f x, px * pf)

-- | Binomial distribution with n experiments and success probability p
binom :: Int -> Prob -> Dist Int
binom n p = foldl1 (\x y -> squishD (liftA2 (+) x y)) $ replicate n (coin p 1 0)

instance Monad Dist where
  -- (>>=) :: Dist a -> (a -> Dist b) -> Dist b
  (Dist xs) >>= f = Dist $ do
    (x, p) <- xs
    (y, p') <- unpackDist (f x)
    return (y, p * p')

-- Conditioning on an Event
-- ========================================

-- | Condition a distribution on an event
condD :: (a -> Bool) -> Dist a -> Dist a
condD f (Dist xs) = Dist . normP $ filter (f . fst) xs
