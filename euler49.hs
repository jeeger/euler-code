module Main where
import Data.List(permutations, unfoldr, sort, nub)

isPrime:: Int -> Bool
isPrime n = all (\y -> n `mod` y /= 0) [2..(floor $ sqrt $ fromIntegral n)]

primes:: [Int]
primes = filter isPrime [2..]

fromDigits:: [Int] -> Int
fromDigits l = join $ reverse l
  where join [] = 0
        join (i:is) = 10 * join is + i

toDigits:: Int -> [Int]
toDigits n = reverse $ unfoldr split n
  where split 0 = Nothing
        split n = Just (n `rem` 10, n `div` 10)

permutPrimes:: Int -> [Int]
permutPrimes cand = nub $ sort $ filter isPrime $ filter ( > cand) permutedNums where
  permutedNums = map fromDigits $ permutations $ toDigits cand

findStep:: Int -> [Int] -> [(Int, Int, Int)]
findStep cand permuts = [(cand, x, y) | x <- permuts, y <- dropWhile (<=x) permuts, 2 * (x - cand) == y - cand]

candidates = dropWhile (<=999) primes

solution = foldr findSolution [] candidates where
  findSolution  v l = let primes = findStep v $ permutPrimes v in
    if primes /= [] then
      primes : l
    else
      l

main = print (take 2 solution)
