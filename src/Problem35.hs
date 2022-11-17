module Problem35 (primeFactors) where

primeFactorsHelper :: Int -> Int -> [Int]
primeFactorsHelper 1 _ = []
primeFactorsHelper num div
    | mod num div == 0 = [div] ++ (primeFactorsHelper (fst $ divMod num div) div)
    | otherwise = primeFactorsHelper num (div+1)


primeFactors :: Int -> [Int]
primeFactors num = primeFactorsHelper num 2
