module Euler46 where

isPrime:: Int -> Bool
isPrime n = all (\y -> n `mod` y /= 0) [2..(floor $ sqrt $ fromIntegral n)]

candidates = filter (\x -> odd x && not (isPrime x)) [1..]

canDeconstruct:: Int -> Bool
canDeconstruct x = or [ True | square <- squares, isPrime (x - 2 * square)] where
  squares = map (\x -> x * x) [1..(floor $ sqrt $ (fromIntegral x) / 2)]

solution = head $ dropWhile canDeconstruct candidates
