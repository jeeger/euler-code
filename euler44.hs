module Euler44 where

isInt x = x == fromInteger (round x)

nToPent:: Int -> Maybe Int
nToPent pn = let n = (sqrt (fromIntegral (24 * pn + 1)) + 1) / 6 in
  if isInt n then
    Just $ fromIntegral $ toInteger $ round n
  else
    Nothing

nthPent:: Int -> Int
nthPent n = fromIntegral $ round  (fromIntegral (n * (3 * n - 1)) / 2)

isPent:: Int -> Bool
isPent n = case nToPent n of
  Nothing -> False
  Just _ -> True

sumDiffIsPent:: Int -> Int -> Bool
sumDiffIsPent x y = isPent (x - y) && isPent (x + y)

pentagonals = map nthPent [1..]
sumDiffPentagonals = [(sumDiffIsPent x y, x, y) | x <- pentagonals, y <- takeWhile (\p -> p < x) pentagonals ]

solution = let (_, x, y) = head $ dropWhile (\(t, _, _) -> not t) sumDiffPentagonals in
  x - y
