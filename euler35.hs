import Data.List (unfoldr, group, intersect)

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


rotations:: Integer -> [[Integer]]
rotations n = let dig = toDigits n
                  len = length dig in
                take len $ chunk len $ cycle dig
  where chunk i l = take i l : chunk i (drop (i + 1) l)


valid:: Integer -> Bool
valid i = let rot = rotations i in
            (all (\x -> intersect x [2, 4, 5, 6, 8, 0] == []) rot)
            && (all isPrime $ map fromDigits rot)
  
  
findValid:: Integer -> [Integer]
findValid max = filter valid [1,2 .. max]
