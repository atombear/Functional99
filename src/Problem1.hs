module Problem1 (mylast) where

mylast :: Eq a => [a] -> a
mylast (x:xs)
    | xs == [] = x
    | otherwise = mylast xs
