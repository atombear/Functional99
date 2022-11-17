module Problem41 (goldbachList, goldbachList') where

import Problem40 (goldbach)

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList start stop = map goldbach [2 * x | x <- [x0..x1]]
    where x0 = (div start 2) + (if (mod start 2 == 1) then 1 else 0)
          x1 = div stop 2


goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' start stop min_sum = filter (\(x,y) -> (min x y) > 50) $ goldbachList start stop