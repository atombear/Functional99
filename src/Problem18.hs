module Problem18 (slice) where


-- slice :: [a] -> Int -> Int -> [a]
-- slice word start stop = [word !! idx | idx <- [start-1..stop-1]]



sliceHelper :: [a] -> Int -> Int -> [a] -> Int -> [a]
sliceHelper (x:xs) start stop cache idx
    | idx > stop = reverse cache
    | idx < start = sliceHelper xs start stop cache (idx + 1)
    | otherwise = sliceHelper xs start stop (x:cache) (idx+1)

slice :: [a] -> Int -> Int -> [a]
slice word start stop = sliceHelper word start stop [] 1