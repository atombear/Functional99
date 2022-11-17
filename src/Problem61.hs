module Problem61 (countLeaves, collectLeaves) where


import Problem55 (Tree (Node, Nil))


countLeaves :: (Eq a) => Tree a -> Int
countLeaves Nil = 0
countLeaves (Node _ left right)
    | left == Nil && right == Nil = 1
    | otherwise = (countLeaves left) + (countLeaves right)


collectLeaves :: (Eq a) => Tree a -> [a]
collectLeaves Nil = []
collectLeaves (Node val left right)
    | left == Nil && right == Nil = [val]
    | otherwise = (collectLeaves left) ++ (collectLeaves right)
