module Problem68 (preOrder, inOrder, treeFromPreIn) where

import Problem55 (Tree (Node, Nil))


inOrder :: Tree Char -> [Char]
inOrder Nil = []
inOrder (Node val left right) = (inOrder left) ++ [val] ++ (inOrder right)


preOrder :: Tree Char -> [Char]
preOrder Nil = []
preOrder (Node val left right) = [val] ++ (preOrder left) ++ (preOrder right)


treeFromPreIn :: [Char] -> [Char] -> Tree Char
treeFromPreIn [] [] = Nil
treeFromPreIn pre_order in_order = Node root left right
    where root = head pre_order
          split = length $ takeWhile (\x -> x /= root) in_order
          left_in = [c | (idx, c) <- zip [0..] in_order, idx < split]
          right_in = [c | (idx, c) <- zip [0..] in_order, idx > split]
          left_pre = filter (\x -> elem x left_in) $ tail pre_order
          right_pre = filter (\x -> elem x right_in) $ tail pre_order
          left = treeFromPreIn left_pre left_in
          right = treeFromPreIn right_pre right_in