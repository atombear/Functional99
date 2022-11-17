module Problem65 (layoutBin, fromList) where

import Problem55 (Tree (Node, Nil), toList)


height :: Tree a -> Int
height Nil = 0
height (Node _ left right) = 1 + (max (height left) (height right))


qsort :: (Eq a, Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort $ filter (\y -> y <= x) xs) ++ [x] ++ (qsort $ filter (\y -> y > x) xs)


getIndex :: Eq a => [a] -> a -> Int
getIndex list elem = length $ takeWhile (\x -> x /= elem) list


applyOrdering :: Eq a => Tree a -> Int -> Int -> Int -> Tree (a, (Int, Int))
applyOrdering Nil _ _ _ = Nil
applyOrdering (Node val left right) antidepth depth xval = Node (val, (xval, 1 + depth))
                                                                (applyOrdering left (antidepth - 1) (depth+1) (xval - offset))
                                                                (applyOrdering right (antidepth - 1) (depth+1) (xval + offset))
    where offset = div (2^antidepth) 4


fromList :: Eq a => [Maybe a] -> Int -> Tree a
fromList list root = if (root_node == Nothing) then Nil else (Node val left right)
    where root_node = list !! root
          (Just val) = root_node
          len_list = length list
          left_node = 2*root + 1
          right_node = 2*root + 2
          left_child = list !! left_node
          right_child = list !! right_node
          left = if (left_node >= len_list) || (left_child == Nothing) then Nil else (fromList list left_node)
          right = if (right_node >= len_list) || (right_child == Nothing) then Nil else (fromList list right_node)


layoutBin :: Ord a => Tree a -> Tree (a, (Int, Int))
layoutBin tree = fromList (map update_x tree_list) 0
    where tree_list = toList $ applyOrdering tree (height tree) 0 0
          min_x = minimum $ map (\(Just x) -> (fst.snd) x) $ filter (\x -> x/=Nothing) tree_list
          update_x Nothing = Nothing
          update_x (Just (v, (x, y))) = Just (v, (1+x-min_x, y))
