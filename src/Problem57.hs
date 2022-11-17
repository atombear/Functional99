module Problem57 (constructBST) where

import Problem55 (Tree (Node, Nil))

constructBST :: Ord a => [a] -> Tree a
constructBST [] = Nil
constructBST (v:xs) = Node v less more
    where less = constructBST $ filter (\x -> x<v) xs
          more = constructBST $ filter (\x -> x>=v) xs