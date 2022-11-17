module Problem17 (split) where


splitHelper :: [a] -> Int -> [a] -> ([a], [a])
splitHelper word 0 cache = (reverse cache, word)
splitHelper (x:xs) num cache = splitHelper xs (num-1) (x:cache)

split :: [a] -> Int -> ([a], [a])
split word num = splitHelper word num []