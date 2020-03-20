import Data.List (unfoldr)
import Data.Set (fromList, size)

toDigits:: Integer -> [Integer]
toDigits n = reverse $ unfoldr split n
  where split 0 = Nothing
        split n = Just (n `rem` 10, n `div` 10)


pandigital:: Integer -> Bool
pandigital x = ((size $ fromList $ toDigits x) == 9)
               && ((length $ show x) == 9)

               
