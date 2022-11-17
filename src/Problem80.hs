module Problem80 (EdgeGraph, AdjGraph, transEdgeToAdj, transAdjToEdge) where


import qualified Data.Map as Map (Map, fromList, map, foldl, mapWithKey)
import qualified Data.Set as Set (Set, fromList, filter, map, union)


-- these are directed graphs
type EdgeGraph a = Set.Set (a, a)
type AdjGraph a = Map.Map a (Set.Set a)


removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates list = foldl (\accum -> \el -> if elem el accum then accum else (el:accum)) [] list


transEdgeToAdj :: Ord a => EdgeGraph a -> AdjGraph a
transEdgeToAdj edge_graph = Map.fromList [(k, get_children k) | k <- elements]
    where elements = removeDuplicates $ concat $ Set.map (\(i, j) -> [i, j]) edge_graph
          get_children parent = Set.map snd $ Set.filter (\(i, j) -> i == parent) edge_graph


transAdjToEdge :: Ord a => AdjGraph a -> EdgeGraph a
transAdjToEdge adj_graph = pairs
    where empty_set = Set.fromList []
          pairs = Map.foldl Set.union empty_set $ Map.mapWithKey (\key -> \values -> Set.map (\v -> (key, v)) values) adj_graph
