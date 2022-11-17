module Problem33 (coprime) where

import Problem32 (myGCD)

coprime :: Int -> Int -> Bool
coprime x y = (myGCD x y == 1)