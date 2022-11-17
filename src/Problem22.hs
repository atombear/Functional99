module Problem22 (range) where


rangeHelper :: Int -> Int -> Int -> [Int]
rangeHelper start stop counter
    | counter > stop = []
    | otherwise = (counter:rest)
    where rest = rangeHelper start stop (counter + 1)


range :: Int -> Int -> [Int]
range start stop = rangeHelper start stop start