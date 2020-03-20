module Euler45 where

isInt x = x == fromInteger (round x)

isPent:: Int -> Bool
isPent pn = isInt ((sqrt (fromIntegral (24 * pn + 1)) + 1) / 6)

isHex:: Int -> Bool
isHex hx = isInt ((sqrt (fromIntegral (8 * hx + 1)) + 1) / 4)

nthTriang:: Int -> Int
nthTriang x = fromIntegral $ round (fromIntegral  (x * (x + 1)) / 2)

triangs:: [Int]
triangs = map nthTriang [1..]

solution = head $ drop 2 $ filter (\x -> (isHex x) && (isPent x)) triangs
