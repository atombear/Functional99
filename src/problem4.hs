module Problem4 (myLength) where

myLength :: [a] -> Int
myLength = foldl (\x -> \y -> x + 1) 0