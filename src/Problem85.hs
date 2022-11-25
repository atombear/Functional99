module Problem85 (iso, isoBrute) where

import qualified Data.Map as Map (elems, keysSet, toList, (!), filter)
import qualified Data.Set as Set (toList, union, fromList, map, difference, member)

import Problem80 (AdjGraph, EdgeGraph, transAdjToEdge, transEdgeToAdj)

import Debug.Trace (trace)

data Cmp = Less | Equal | More deriving (Show, Eq)


sortWithKey :: [a] -> (a -> a -> Cmp) -> [a]
sortWithKey [] _ = []
sortWithKey [val] _ = [val]
sortWithKey (list@(x:xs)) key
    | otherwise = (sortWithKey lt key) ++ [pivot] ++ (sortWithKey gt key)
    where pivot = x
          lt = filter (\y -> y `key` pivot  == Less || y `key` pivot == Equal) xs
          gt = filter (\y -> y `key` pivot == More) xs


productLists :: [[[a]]] -> [[a]]
productLists [] = []
productLists [val] = val
productLists (x:xs) = [i ++ j | i <- x, j <- productLists xs]


findUnique :: Eq a => [a] -> [a]
findUnique list = foldl (\accum -> \el -> if elem el accum then accum else (el:accum)) [] list


permutations :: [a] -> [[a]]
permutations list
    | length list == 0 = []
    | length list == 1 = [list]
    | otherwise = concat $ map (\(el, slist) -> map (el:) $ permutations slist) el_rm
    where el_rm = [(list !! idx, getListWithoutEl idx) | idx <- [0..(length list) - 1]]
          getListWithoutEl idx = [el | (jdx, el) <- zip [0..] list, jdx /= idx]


getIndex :: Eq a => [a] -> a -> Int
getIndex (x:xs) el = if x == el then 0 else 1 + getIndex xs el


removeEl :: Eq a => [a] -> a -> [a]
removeEl list el = [e | e <- list, e /= el]


swapLabels :: Ord a => AdjGraph a -> [a] -> [a] -> AdjGraph a
swapLabels adj_graph src dst = transEdgeToAdj translation
    where lookup el = dst !! (getIndex src el)
          edge_graph = transAdjToEdge adj_graph
          translation = Set.map (\(i,j) -> (lookup i, lookup j)) edge_graph


isoBrute :: Ord a => AdjGraph a -> AdjGraph a -> Bool
isoBrute g0 g1 = any ((==)g0) (map (swapLabels g1 nodes1) unique_perms)
    where empty_set = Set.fromList []
          key_set = Map.keysSet g1
          nodes1 = Set.toList $ key_set `Set.union` (foldl Set.union empty_set (Map.elems g1))
          unique_perms = findUnique $ permutations nodes1
          winner = head [up | up <- unique_perms, swapLabels g1 nodes1 up == g0]


numParents :: Ord a => AdjGraph a -> a -> Int
numParents graph el = length $ Map.filter (\x -> Set.member el x) graph


iso' :: (Ord a, Ord b) => AdjGraph a -> AdjGraph b -> [a] -> [b] -> Bool
iso' _ _ [] [] = True
iso' g0 g1 nodes0 nodes1 = any id $ map check nodes1
    where el0 = head nodes0
          checkEl1 el1 = ((length $ g0 Map.! el0) == (length $ g1 Map.! el1)) && ((numParents g0 el0) == (numParents g1 el1))
          checkRest el1 = iso' g0 g1 (tail nodes0) (removeEl nodes1 el1)
          check el1 = (checkEl1 el1) && (checkRest el1)


iso :: (Ord a, Ord b) => AdjGraph a -> AdjGraph b -> Bool
iso g0 g1 = iso' g0 g1 nodes0 nodes1
    where key_set0 = Map.keysSet g0
          nodes0 = Set.toList $ key_set0 `Set.union` (foldl Set.union (Set.fromList []) (Map.elems g0))
          key_set1 = Map.keysSet g1
          nodes1 = Set.toList $ key_set1 `Set.union` (foldl Set.union (Set.fromList []) (Map.elems g1))
