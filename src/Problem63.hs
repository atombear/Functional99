module Problem63 (completeBinaryTree, isCompleteBinaryTree) where

import Problem55 (Tree (Node, Nil), toList)


getLargestFill :: Int -> Int
getLargestFill nodes = (last . (takeWhile (\x -> x <= nodes))) [2^n - 1 | n <- [0..]]


getExtraLR :: Int -> (Int, Int)
getExtraLR nodes = if rem > half_level then (half_level, rem - half_level) else (rem, 0)
    where fill = getLargestFill nodes
          next_level = 1 + fill
          half_level = div next_level 2
          rem = nodes - fill


completeBinaryTree :: Int -> Tree Char
completeBinaryTree 0 = Nil
completeBinaryTree 1 = Node 'x' Nil Nil
completeBinaryTree n = Node 'x' left right
    where fill = getLargestFill n
          (l, r) = getExtraLR n
          left  = completeBinaryTree ((div (fill-1) 2) + l)
          right = completeBinaryTree ((div (fill-1) 2) + r)


stripLeft :: Eq a => [a] -> a -> [a]
stripLeft [] _ = []
stripLeft list@(x:xs) v
    | x == v = stripLeft xs v
    | otherwise = list


isCompleteBinaryTree :: Eq a => Tree a -> Bool
isCompleteBinaryTree tree = not $ elem False (stripLeft bins False)
    where bins = reverse $ map (\x -> if x == Nothing then False else True) $ toList tree
