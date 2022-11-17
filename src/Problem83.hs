module Problem83 (det, isTree, countKirchoff, spanningTrees, processGraph) where

import qualified Data.Set as Set (Set, fromList, filter, union, map, insert, toList, member)
import qualified Data.Map as Map ((!), mapWithKey, toList, foldl)

import Problem80 (AdjGraph, transAdjToEdge, transEdgeToAdj)
import Problem81 (paths)


getNodes :: Ord a => AdjGraph a -> Set.Set a
getNodes graph = Map.foldl Set.union (Set.fromList []) $ Map.mapWithKey (\k -> \v -> Set.insert k v) graph


removeEl :: [a] -> Int -> [a]
removeEl arr idx = [el | (jdx, el) <- zip [0..] arr, jdx /= idx]

removeRC :: [[a]] -> Int -> [[a]]
removeRC mat idx = map (\x -> removeEl x idx) (removeEl mat 0)

det :: Num a => [[a]] -> a
det [[v]] = v
det arr = sum [((head arr) !! n) * ((-1)^n) * (det $ removeRC arr n) | n <- [0..((length arr) -1)]]


processGraph :: Ord a => AdjGraph a -> AdjGraph a
processGraph graph = transEdgeToAdj edge_incl_inv
    where edge_graph = transAdjToEdge graph
          edge_rm_self = Set.filter (\(x, y) -> x /= y) edge_graph
          edge_incl_inv = Set.union edge_rm_self (Set.map (\(i, j) -> (j, i)) edge_rm_self)



isTree :: Ord a => AdjGraph a -> Bool
isTree graph = connected && node_edge_cond
    where pgraph = processGraph graph
          entry = (fst.head.(Map.toList)) pgraph
          nodes = getNodes pgraph
          edges = transAdjToEdge pgraph
          node_edge_cond = (length nodes) - 1 == div (length edges) 2
          node_list = Set.toList nodes
          connected = all id $ [1 == (length $ paths i j pgraph) | i <- node_list, j <- node_list, i /= j]


indexAt :: Eq a => a -> [a] -> Int
indexAt val (x:xs) = if x == val then 0 else 1 + (indexAt val xs)

countKirchoff :: Ord a => AdjGraph a -> Int
countKirchoff graph = det $ removeRC (map getRow node_list) 0
    where pgraph = processGraph graph
          node_list = Set.toList $ getNodes pgraph
          diag = map (\x -> length $ pgraph Map.! x) node_list
          getRow ni = [if ni == nj then
                           (diag !! (indexAt ni node_list))
                       else if Set.member nj (pgraph Map.! ni) then
                           -1
                       else
                           0
                       | nj <- node_list]


allCombinations :: [a] -> [[a]]
allCombinations (list@(x:xs))
    | length list == 0 = [[]]
    | length list == 1 = [list, []]
    | otherwise = concat [[l, (x:l)] | l <- allCombinations xs]


getUnique :: Eq a => [a] -> [a]
getUnique list = foldl (\accum -> \el -> if elem el accum then accum else (el:accum)) [] list


spanningTrees :: Ord a => AdjGraph a -> [AdjGraph a]
spanningTrees graph = filter (\x -> isTree x && length x == length graph) all_graphs
    where pgraph = processGraph graph
          edge_list = Set.toList $ transAdjToEdge pgraph
          one_way = getUnique [if i<j then (i, j) else (j, i) | (i, j) <- edge_list]
          combs = allCombinations one_way
          all_edge_lists = map (\comb -> [(i, j) | (i, j) <- edge_list, elem (i, j) comb || elem (j, i) comb]) combs
          all_graphs = map transEdgeToAdj $ map Set.fromList $ all_edge_lists
