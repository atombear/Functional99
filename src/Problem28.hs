module Problem28 where

import Data.Map (Map, adjust, fromList, toList, member, insert)


adjustWithDefault :: Ord k => (a -> a) -> k -> a -> Map k a -> Map k a
adjustWithDefault f key val map
    | member key map = adjust f key map
    | otherwise = insert key val map


qsort :: (Eq a, Ord a) => [a] -> (a -> a -> Bool) -> [a]
qsort [] _ = []
qsort list key = left ++ [pivot] ++ right
    where pivot = head list
          left = qsort [i | i <- tail list, key i pivot] key
          right = qsort [i | i <- tail list, not $ key i pivot] key


lsort :: (Eq a, Ord a) => [[a]] -> [[a]]
lsort list = qsort list (\x -> \y -> (length x) <= (length y))


lfsort :: (Eq a, Ord a) => [[a]] -> [[a]]
lfsort list = foldl (\x -> \y -> x ++ [v | v <- list, length v == y]) [] ordered_lengths
    where length_map = foldl (\x -> \y -> adjustWithDefault (+1) (length y) 1 x) (fromList []) list
          ordered_lengths = map fst $ qsort (toList length_map) (\x -> \y -> snd x <= snd y)
