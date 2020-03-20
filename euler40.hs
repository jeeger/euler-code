import Data.List (unfoldr)
import Data.Foldable (foldl')

digitLen:: Int -> Int
digitLen 0 = 0
digitLen n = succ (digitLen (n `div` 10))

toDigits:: Int -> [Int]
toDigits n = reverse $ unfoldr split n
  where split 0 = Nothing
        split n = Just (n `rem` 10, n `div` 10)


champernowne:: [Int]
champernowne = concatMap toDigits $ iterate succ 1

toSelect:: [Int]
toSelect = [1, 10, 100, 1000, 10000, 100000, 1000000]

selectDigits:: [Int] -> [Int] -> [Int]
selectDigits (i:is) d = let newd = drop (pred i) d
                            newis = fmap (\x -> x + 1 - i) is
                        in
                          head newd : selectDigits newis newd
selectDigits [] d = []

answer = foldl' (*) 1 (selectDigits toSelect (champernowne))
