module Main where
import Data.List(unfoldr)

isqrt = floor . sqrt . fromIntegral

isPrime:: Int -> Bool
isPrime n = null [ x | x <- [2..isqrt n], n `mod` x == 0]

fromDigits:: [Int] -> Int
fromDigits l = join $ reverse l
  where join [] = 0
        join (i:is) = 10 * join is + i

toDigits:: Int -> [Int]
toDigits n = reverse $ unfoldr split n
  where split 0 = Nothing
        split n = Just (n `rem` 10, n `div` 10)

expand:: Int -> [Int]
expand n = concat $ map (insert (toDigits n)) [0..9]  where
  insert d n = map fromDigits [take x1 d ++ [n] ++ take x2 (drop x1 d) ++ [n] ++ (drop (x1 + x2) d) | x1 <- [0..length d], x2 <- [x1..length d]]

main = undefined
