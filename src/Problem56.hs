module Problem56 (symmetricTree) where

import Problem55 (Tree (Node, Nil))


mirror :: Tree a -> Tree a -> Bool
mirror Nil Nil = True
mirror Nil _ = False
mirror _ Nil = False
mirror (Node _ Nil Nil) (Node _ Nil Nil) = True
mirror (Node _ ll lr) (Node _ rl rr) = (mirror ll rr) && (mirror lr rl)


symmetricTree :: Tree a -> Bool
symmetricTree Nil = True
symmetricTree (Node _ left right) = mirror left right
