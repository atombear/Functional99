module Problem26 (combinations, choose) where

flatten :: [[a]] -> [a]
flatten = foldl (++) []

factorial :: Int -> Int
factorial 0 = 0
factorial 1 = 1
factorial n = foldl (*) 1 [1..n]

choose :: Int -> Int -> Int
choose n k
    | k == 0 = 1
    | k == n = 1
    | otherwise = (div (div (factorial n) (factorial k)) (factorial (n - k)))

combinationsIdx :: Int -> Int -> Int -> [[Int]]
combinationsIdx start stop 1 = [[idx] | idx <- [start..(stop-1)]]
combinationsIdx start stop n = flatten [[(start_idx:list) | list <- combinationsIdx (start_idx + 1) stop (n-1)] | start_idx <- [start..(stop)]]

combinations :: Int -> [a] -> [[a]]
combinations num list = map (\x -> [list !! idx | idx <- x]) idxs
    where idxs = combinationsIdx 0 (length list) num