module Problem5 (myReverse) where

myReverse :: Foldable t => t a -> [a]
myReverse = foldl (\x -> \y -> y:x) []