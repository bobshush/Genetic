module Genetic where

import Data.List
import System.Random
import Control.Monad

type PoolSize = Int
type SurvivorSize = Int
type Generations = Integer

takeRandom :: [a] -> IO a
takeRandom x = do
 r <- randomRIO (0, length x - 1)
 return (x !! r)

class Evolvable a where
 mutate :: a -> IO a
 reproduce :: a -> a -> IO a
 fitness :: a -> Double
 breed :: Int -> [a] -> IO [a]
 breed num x = sequence $ take num $ repeat $ ((join $ liftM2 reproduce (takeRandom x) (takeRandom x)) >>= mutate)
 evolve :: PoolSize -> SurvivorSize -> Integer -> [a] -> IO [a] 
 evolve pools survs 0 a = return (sortBy (\x y -> if (fitness x) < (fitness y) then GT else LT) a)
 evolve pools survs gens a = best where
  firstpool = a
  survivors = take survs $ sortBy (\x y -> if (fitness x) < (fitness y) then GT else LT) firstpool
  newspawn = do
   putStrLn $ "Remaining Generations:" ++ (show gens)
   newguys <- (breed (pools - survs) survivors)
   return (newguys ++ (take 1 survivors))
  nextgen = newspawn >>= evolve pools survs (gens - 1)
  best = nextgen

 evolveUntil :: PoolSize -> SurvivorSize -> Double -> [a] -> IO [a]
 evolveUntil p s d a = evolveUntilCount p s 0 d a

 evolveUntilCount :: PoolSize -> SurvivorSize -> Int -> Double -> [a] -> IO [a]
 evolveUntilCount pools survs count limit a = best where
  firstpool = a
  survivors = take survs $ sortBy (\x y -> if (fitness x) < (fitness y) then GT else LT) firstpool
  newspawn = (liftM (survivors ++))  (breed (pools - survs) survivors)
--  nextgen = newspawn >>= evolveUntilCount pools survs (count+1) limit
  fittest = head $ sortBy (\x y -> if (fitness x) < (fitness y) then GT else LT) firstpool
  best = do
   fit <- return (fitness fittest)
   zeeroo <- if (count `mod` 1 == 0) then do
     putStrLn (show fit)
     return 0 
    else return 0
   if (fit >= limit) then return [fittest] else (newspawn >>= evolveUntilCount pools survs (count+1) limit)
 

class Preturbable a where
 preturb :: Double -> Double -> a -> IO a --Probability, how much max

class Reproducible a where 
 mate :: a -> a -> IO a

instance Preturbable Double where
 preturb p m a = do
  r <- randomIO :: IO Double
  add <- if r < p then (randomRIO (-1 * m, m) :: IO Double) else return 0
  return (a + add)

instance Reproducible Double where
 mate a b = randomRIO (a,b)

instance Reproducible Float where
 mate a b = randomRIO (a,b)

instance Preturbable Bool where
 preturb p m a = do
  r <- randomIO :: IO Double
  b <- if r < p then (return $ not a) else return a
  return b

instance Reproducible Bool where
 mate a b = randomRIO (a,b)
