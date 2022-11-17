module Problem64 (layout) where

import Problem55 (Tree (Node, Nil), toList)


height :: Tree a -> Int
height Nil = 0
height (Node _ left right) = 1 + (max (height left) (height right))


qsort :: (Eq a, Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort $ filter (\y -> y <= x) xs) ++ [x] ++ (qsort $ filter (\y -> y > x) xs)


getIndex :: Eq a => [a] -> a -> Int
getIndex list elem = length $ takeWhile (\x -> x /= elem) list


applyOrdering :: Eq a => Tree a -> [a] -> Int -> Tree (a, (Int, Int))
applyOrdering Nil _ _ = Nil
applyOrdering (Node val left right) lookup depth = Node (val, (1 + getIndex lookup val, 1 + depth))
                                                        (applyOrdering left lookup (depth+1))
                                                        (applyOrdering right lookup (depth+1))


layout :: Ord a => Tree a -> Tree (a, (Int, Int))
layout tree = applyOrdering tree lookup 0
    where unMaybe list = map (\(Just x) -> x) $ filter (\x -> x /= Nothing) list
          lookup = qsort $ unMaybe $ toList tree
