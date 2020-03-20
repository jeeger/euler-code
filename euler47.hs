module Main where

isPrime:: Int -> Bool
isPrime n = all (\y -> n `mod` y /= 0) [2..(floor $ sqrt $ fromIntegral n)]

primes = filter isPrime [2..]

primeFactors:: Int -> [Int]
primeFactors n = filter (\f -> n `mod` f == 0) $ takeWhile (\x -> fromIntegral x < n) primes

sliding :: Int -> [a] -> [[a]]
sliding size [] = []
sliding size ls@(x:xs) = (take size ls) : sliding size xs

candidates factors = [num | num <- [3..], length (primeFactors num) == factors]

consecutive:: [Int] -> Bool
consecutive l = l == [head l..last l]

solution factors = head $ filter consecutive $ sliding factors $ candidates factors

main = print (solution 4)
