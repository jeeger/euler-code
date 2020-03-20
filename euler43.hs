module Euler43 where
import Data.List(permutations, unfoldr, sort)

fromDigits:: [Int] -> Int
fromDigits l = join $ reverse l
  where join [] = 0
        join (i:is) = 10 * join is + i

toDigits:: Int -> [Int]
toDigits n = reverse $ unfoldr split n
  where split 0 = Nothing
        split n = Just (n `rem` 10, n `div` 10)


-- TODO: Understand this.
sublists n xs = take (length xs - n + 1) $ sublists' n xs
    where sublists' _ [] = [[]]
          sublists' n xs@(_:rest) = take n xs : sublists' n rest

digits = [0..9]
-- Filter out permutations with leading zero
all_pandigital:: [[Int]]
all_pandigital = filter (\x -> (head x) /= 0) $ permutations digits

factorOffsets = [(2, 1), (3, 2), (5, 3), (7, 4), (11, 5), (13, 6), (17, 7)]

sublist:: [a] -> Int -> Int -> [a]
sublist list start length = take length $ drop start list

hasProperty l = all matchesProperty factorOffsets where
  matchesProperty (factor, offset) = fromDigits (sublist l offset 3) `mod` factor == 0

solution = sum $ map fromDigits $ filter hasProperty all_pandigital
