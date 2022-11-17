module Problem86 (degree, degreeSort, colorWelshPowell) where

import qualified Data.Set as Set (filter, union, fromList, toList, member)
import qualified Data.Map as Map (keysSet, foldl, (!))
import Control.Monad.State (State, execState, get, put)

import Problem80 (AdjGraph, EdgeGraph, transAdjToEdge)
import Problem83 (processGraph)


degree :: Ord a => AdjGraph a -> a -> Int
degree graph node = (\x -> div x 2) $ length $ Set.filter (\(i,j) -> i == node || j == node) edge_graph
    where edge_graph = transAdjToEdge $ processGraph graph


sortWithKey :: [a] -> (a -> a -> Bool) -> [a]
sortWithKey [] _ = []
sortWithKey [val] _ = [val]
sortWithKey (pivot:xs) lt_key = left ++ [pivot] ++ right
    where left = sortWithKey (filter (not.(lt_key pivot)) xs) lt_key
          right = sortWithKey (filter (lt_key pivot) xs) lt_key


degreeSort :: Ord a => AdjGraph a -> [a]
degreeSort graph = sortWithKey nodes (\x -> \y -> degree graph x > degree graph y)
    where empty_set = Set.fromList []
          nodes = Set.toList $ (Map.keysSet graph) `Set.union` (Map.foldl Set.union empty_set graph)


colorWP :: Ord a => AdjGraph a -> [a] -> State ([(a, Int)], Int) ()
colorWP _ [] = return ()
colorWP graph (x:xs) = do
    (cmap, color) <- get
    let cnodes = fst $ foldl (\(accum, children) -> \el ->
                             if el `Set.member` children then
                                 (accum, children)
                             else
                                 ((el:accum), children `Set.union` (graph Map.! el)))
                       ([x], graph Map.! x)
                       xs
        next_nodes = [n | n <- xs, not $ n `elem` cnodes]
        next_cmap = cmap ++ [(n, color) | n <- cnodes]
    put (next_cmap, color+1)
    colorWP graph next_nodes


colorWelshPowell :: Ord a => AdjGraph a -> [(a, Int)]
colorWelshPowell graph = fst $ execState (colorWP pgraph sorted_nodes) ([], 0)
    where pgraph = processGraph graph
          sorted_nodes = degreeSort graph
