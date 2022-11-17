module Problem32 (myGCD) where

myGCD :: Int -> Int -> Int
myGCD m n
    | m == n = abs n
    | otherwise = gcd (abs(am-an)) (min am an)
    where am = abs m
          an = abs n
