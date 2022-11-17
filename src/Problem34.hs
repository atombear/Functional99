module Problem34 (totient) where

import Problem33 (coprime)


totient :: Int -> Int
totient 1 = 1
totient x = length $ filter (coprime x) [1..(x-1)]
