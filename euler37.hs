import Data.List (unfoldr, inits, tails)
import Data.Set (fromList, member)

isPrime:: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x = ([] == [y | y <- 2: [3, 5 .. floor $ sqrt $ fromIntegral x], x `mod` y == 0])

toDigits:: Integer -> [Integer]
toDigits n = reverse $ unfoldr split n
  where split 0 = Nothing
        split n = Just (n `rem` 10, n `div` 10)

fromDigits:: [Integer] -> Integer
fromDigits l = join $ reverse l
  where join [] = 0
        join (i:is) = 10 * join is + i


prefixesAndSuffixes:: [a] -> [[a]]
prefixesAndSuffixes l = l: unfoldr suf l ++ unfoldr pref l
  where suf [x] = Nothing
        suf l = Just (tail l, tail l)
        pref [x] = Nothing
        pref l = Just (init l, init l)

none:: Foldable t => (a -> Bool) -> t a -> Bool
none f l = all (not . f) l

canBeTruncPrime:: [Integer] -> Bool
canBeTruncPrime l = not (member (head l) (fromList 


truncPrime:: Integer -> Bool
truncPrime x = let truncations = prefixesAndSuffixes $ toDigits x in
                 (all isPrime $ map fromDigits truncations)


findElevenTruncPrimes:: Integer -> [Integer]
findElevenTruncPrimes max = take 11 $ filter truncPrime [11, 13 .. max]
