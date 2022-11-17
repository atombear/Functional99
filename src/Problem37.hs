module Problem37 (phi) where


import Problem36 (prime_factors_mult)


phi :: Int -> Int
phi num = foldl (\x -> \(y0, y1) -> x * ((y0-1) * (y0 ^ (y1-1)))) 1 $ prime_factors_mult num