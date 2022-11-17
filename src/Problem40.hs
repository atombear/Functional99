module Problem40 (goldbach) where

import Problem31 (isPrime)


toTuple :: [Int] -> (Int, Int)
toTuple list = (head list, head $ tail list)

goldbach :: Int -> (Int, Int)
goldbach n = toTuple $ head $ filter (all isPrime) $ map (\x -> [x, n - x]) [2..1 + (div n 2)]
