module Problem55 (cbalTree, Tree (Node, Nil), height, toList) where

import Control.Monad.State (State, execState, get, put)

data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Eq)


height :: Tree a -> Int
height Nil = 0
height (Node x left right) = 1 + (max (height left) (height right))


replaceVal :: [a] -> Int -> a -> [a]
replaceVal (x:xs) 0 val = (val:xs)
replaceVal (x:xs) n val = (x:(replaceVal xs (n-1) val))


fillList :: Tree a -> Int -> State [Maybe a] ()
fillList Nil idx = return ()
fillList (Node val left right) idx = do
    list <- get
    put $ replaceVal list idx (Just val)
    fillList left (2*idx+1)
    fillList right (2*idx+2)
    

toList :: Tree a -> [Maybe a]
toList Nil = []
toList tree = execState (fillList tree 0) list
    where h = height tree
          list = [Nothing | _ <- [0..(2^h)-2]]


cbalTree :: Int -> [Tree Char]
cbalTree 0 = []
cbalTree 1 = [Node 'x' Nil Nil]
cbalTree 2 = [(Node 'x') (Node 'x' Nil Nil) Nil, (Node 'x') Nil (Node 'x' Nil Nil)]
cbalTree n
    | odd n = map node_from_children [(l, r) | l <- cbalTree (div n 2), r <- cbalTree (div n 2)]
    | otherwise = map (\(l, r) -> (Node 'x') l r) ([(l, r) | l <- tree_n0, r <- tree_n1] ++ [(l, r) | l <- tree_n1, r <- tree_n0])
    where node_from_children = (\(l, r) -> (Node 'x') l r)
          n0 = div n 2
          n1 = n0 - 1
          tree_n0 = cbalTree n0
          tree_n1 = cbalTree n1
