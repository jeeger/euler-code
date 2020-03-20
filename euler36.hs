import Data.List (unfoldr)

toBin:: Integer -> [Integer]
toBin 0 = []
toBin x = if x `mod` 2 == 1 then
            1 : (toBin $ x `div` 2)
          else
            0 : (toBin $ x `div` 2)

toDigits:: Integer -> [Integer]
toDigits n = reverse $ unfoldr split n
  where split 0 = Nothing
        split n = Just (n `rem` 10, n `div` 10)


doublePalindrome:: Integer -> Bool
doublePalindrome x = let bin = toBin x
                         dec = toDigits x in
                       bin == (reverse bin) &&
                       dec == (reverse dec)

sumPalindromes:: Integer -> Integer
sumPalindromes max = sum $ filter doublePalindrome [1..max]
