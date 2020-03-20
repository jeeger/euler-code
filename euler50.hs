module Main where
import qualified Data.Set as Set

isPrime:: Int -> Bool
isPrime n = all (\y -> n `mod` y /= 0) [2..(floor $ sqrt $ fromIntegral n)]

solve:: Int -> [Int] -> Int
solve max primes = findSolution primes 0 0 where
  findSolution [] lastprime lastlength = lastprime
  findSolution primes lastprime lastlength = let (s, l) = maxprimesum 0 0 0 0 primes in
    if l > lastlength then
      findSolution (tail primes) s l
    else
      findSolution (tail primes) lastprime lastlength
  maxprimesum sum len lastsum lastlen (p:ps) | nextsum > max = (sum, len)
                                             | nextsum `Set.member` primeset = maxprimesum nextsum nextlen nextsum nextlen ps
                                             | otherwise = maxprimesum sum len nextsum nextlen ps
                                             where nextsum = lastsum + p
                                                   nextlen = lastlen + 1
  maxprimesum sum len lastsum lastlen []  = (sum, len)
  primeset = Set.fromList primes

computeAndSolve max = solve max (filter isPrime [2..max])

main = print $ computeAndSolve (10^6)

