module Problem59 (hbalTree, isBalanced) where

import Problem55 (Tree (Node, Nil))


height :: Tree a -> Int
height Nil = 0
height (Node _ left right) = 1 + (max (height left) (height right))


isBalanced :: Tree a -> Bool
isBalanced Nil = True
isBalanced (Node _ left right) = this_node && (isBalanced left) && (isBalanced right)
    where this_node = (abs ((height left) - (height right))) < 2


hbalTree :: Int -> [Tree Char]
hbalTree 0 = [Nil]
hbalTree 1 = [Node 'x' Nil Nil]
hbalTree n = (treeList (product treesN1 treesN1)) ++ (treeList (product treesN1 treesN2)) ++ (treeList (product treesN2 treesN1))
    where treeList = map (\(l, r) -> Node 'x' l r)
          product list0 list1 = [(i, j) | i <- list0, j <- list1]
          treesN1 = hbalTree (n-1)
          treesN2 = hbalTree (n-2)
