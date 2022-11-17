module Problem16 (dropEvery) where

dropEvery :: [a] -> Int -> [a]
dropEvery word num = [c | (idx, c) <- zip [1..] word, mod idx num /= 0]
