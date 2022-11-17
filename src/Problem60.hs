module Problem60 (hbalTreeNodes) where

import Problem55 (Tree (Node, Nil))


height :: Tree a -> Int
height Nil = 0
height (Node _ left right) = 1 + (max (height left) (height right))

fix :: (a -> a) -> a
fix f = f (fix f)

memo :: (Int -> a) -> (Int -> a)
memo f = ((map f [0..]) !!)

fix_fib :: (Int -> Int) -> (Int -> Int)
fix_fib f 0 = 1
fix_fib f 1 = 1
fix_fib f n = (f (n-1)) + (f (n-2))

fib :: Int -> Int
fib = fix (memo . fix_fib)


-- hbalTreeNodes :: Int -> [Tree Char]
-- hbalTreeNodes 0 = [Nil]
-- hbalTreeNodes 1 = [Node 'x' Nil Nil]
-- hbalTreeNodes n = map (\(left, right) -> Node 'x' left right) all_pairs
--     where pairs = [(l, n-1-l) | l <- [0..n-1]]
--           getTreePairs = \(l, r) -> [(tl, tr) | tl <- hbalTreeNodes l, tr <- hbalTreeNodes r, (abs ((height tl) - (height tr))) < 2]
--           all_pairs = foldl (++) [] $ map getTreePairs pairs

fix_hbalTreeNodes :: (Int -> [Tree Char]) -> (Int -> [Tree Char])
fix_hbalTreeNodes f 0 = [Nil]
fix_hbalTreeNodes f 1 = [Node 'x' Nil Nil]
fix_hbalTreeNodes f n = map (\(left, right) -> Node 'x' left right) all_pairs
    where pairs = [(l, n-1-l) | l <- [0..n-1]]
          getTreePairs = \(l, r) -> [(tl, tr) | tl <- f l, tr <- f r, (abs ((height tl) - (height tr))) < 2]
          all_pairs = foldl (++) [] $ map getTreePairs pairs

hbalTreeNodes :: Int -> [Tree Char]
hbalTreeNodes = fix (memo . fix_hbalTreeNodes)
