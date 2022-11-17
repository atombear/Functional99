module Problem58 (symCbalTrees) where

import Problem55 (Tree (Node, Nil), cbalTree)
import Problem56 (symmetricTree)


symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = filter symmetricTree $ cbalTree n
