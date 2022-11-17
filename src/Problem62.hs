module Problem62 (internals, atLevel) where

import Problem55 (Tree (Node, Nil))

internals :: (Eq a) => Tree a -> [a]
internals Nil = []
internals (Node val left right)
    | left == Nil && right == Nil = []
    | otherwise = [val] ++ (internals left) ++ (internals right)


atLevelHelper :: Tree a -> Int -> Int -> [a]
atLevelHelper Nil level idx = []
atLevelHelper (Node val left right) level idx
    | level < idx = []
    | level == idx = [val]
    | level > idx = (atLevelHelper left level (idx+1)) ++ (atLevelHelper right level (idx+1))


atLevel :: Tree a -> Int -> [a]
atLevel tree level = atLevelHelper tree level 1