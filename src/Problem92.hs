module Problem92 (vonKoch, EdgeGraphUF, checkSolnUF, NodeUF (NodeU, NodeF), updateNode) where

-- NB this may not be universally referred to as von Koch - but i have seen it called 'graceful labelling' and Ringel–Kotzig
-- conjecture being the claim that all trees can be labelled gracefully.


import qualified Data.Set as Set (fromList, toList, Set, map, foldl, insert, union, filter, difference, member)
import Data.List (sort)

import Problem80 (AdjGraph, transEdgeToAdj)
import Problem83 (processGraph)
import Problem85 (iso)


-- The nodes of the graph have either been overwritten by a final value or remain unfilled.
data NodeUF a b = NodeU a | NodeF b deriving (Eq, Show, Ord)

-- We deal with the edge representation.
type EdgeGraphUF a b = Set.Set (NodeUF a b, NodeUF a b)
type NodeUFPair a b = (NodeUF a b, NodeUF a b)

-- determine if a node is filled, or not.
isFilled :: NodeUF a b -> Bool
isFilled (NodeF _) = True
isFilled (NodeU _) = False

isUnfilled :: NodeUF a b -> Bool
isUnfilled = (not.isFilled)


-- is a node an element of a tuple.
elemTuple :: Eq a => a -> (a, a) -> Bool
elemTuple el (x, y) = (el == x) || (el == y)


-- given a node and a targeted difference, determine the possible adjacent values.
getAdjVals :: Integral b => b -> b -> b -> Set.Set b
getAdjVals node_val edge_diff num_nodes = Set.fromList $ filter in_range values
    where in_range i = (0 < i && i <= num_nodes)
          values = [node_val + edge_diff, node_val - edge_diff]


-- replace a node in the graph.
updateNode :: (Ord a, Integral b) => EdgeGraphUF a b -> NodeUF a b -> NodeUF a b -> EdgeGraphUF a b
updateNode graph nodeu nodef = Set.map (\(i, j) -> (swap i, swap j)) graph
    where swap i = (if i == nodeu then nodef else i)


-- get all the filled nodes.
getFilled :: (Ord a, Integral b) => EdgeGraphUF a b -> Set.Set (NodeUF a b)
getFilled graph = Set.foldl (\accum -> \el -> Set.union accum $ capture el) empty_list graph
    where empty_list = Set.fromList []
          capture (x, y) = (Set.fromList $ filter isFilled [x, y])


-- get all the unfilled nodes.
getUnfilled :: (Ord a, Integral b) => EdgeGraphUF a b -> Set.Set (NodeUF a b)
getUnfilled graph = Set.foldl (\accum -> \el -> Set.union accum $ capture el) empty_list graph
    where empty_list = Set.fromList []
          capture (x, y) = Set.fromList $ filter isUnfilled [x, y]


-- get all the unfilled nodes adjacent to a given node in the graph.
getAdjUnfilled :: (Ord a, Integral b) => EdgeGraphUF a b -> NodeUF a b -> Set.Set (NodeUF a b)
getAdjUnfilled graph nodef = Set.filter isUnfilled $ Set.map (\(i, j) -> if i == nodef then j else i) $ Set.filter (elemTuple nodef) graph


-- get all possible fills for a graph that introduce a given edge difference.
getReplacements :: (Ord a, Integral b) => EdgeGraphUF a b -> b -> b -> Set.Set (NodeUFPair a b)
getReplacements graph edge_diff num_nodes = transforms
    where empty_set = Set.fromList []
          filled_raw = (Set.map (\(NodeF v) -> v) $ getFilled graph)
          -- mapping every filled value to the unplaced values that would introduce the given edge difference.
          options = Set.map (\el -> (el, Set.difference (getAdjVals el edge_diff num_nodes) filled_raw)) filled_raw
          -- all the new possible edges that could exist.
          options_pairs = Set.foldl (\accum -> \(i, sj) -> Set.union accum (Set.map (\j -> (i, j)) sj)) empty_set options
          -- mapping a proposed edge to an update in the graph.
          get_unfilled i j = Set.map (\x -> (x, NodeF j)) $ getAdjUnfilled graph (NodeF i)
          transforms = Set.foldl Set.union (Set.fromList []) $ Set.map (\(i,j) -> get_unfilled i j) options_pairs


getDoubleReplacements :: (Ord a, Integral b) => EdgeGraphUF a b -> b -> b -> Set.Set (NodeUFPair a b, NodeUFPair a b)
getDoubleReplacements graph edge_diff num_nodes = Set.fromList ret
    where filled_raw = (Set.map (\(NodeF v) -> v) $ getFilled graph)
          unfilled_edges = Set.toList $ Set.filter (\(n0, n1) -> isUnfilled n0 && isUnfilled n1) graph
          possible_edges = takeWhile (\(_, j) -> j <= num_nodes) [(i, i+edge_diff) | i <- [1..]]
          edges = map (\(i, j) -> (NodeF i, NodeF j)) $ filter (\(i, j) -> not (Set.member i filled_raw || Set.member j filled_raw)) possible_edges
          ret = concat $ [[((u0, f0), (u1, f1)), ((u0, f1), (u1, f0))] | (u0, u1) <- unfilled_edges, (f0, f1) <- edges]


-- solve the graph one edge difference at a time.
vonKoch' :: (Ord a) => EdgeGraphUF a Int -> Int -> Int -> [EdgeGraphUF a Int]
vonKoch' graph edge_diff num_nodes
    | edge_diff == 0 = [graph]
    | otherwise = concat $ map (\g -> vonKoch' g (edge_diff-1) num_nodes) (next_graphs ++ next_dgraphs)
    where repls = getReplacements graph edge_diff num_nodes
          drepls = getDoubleReplacements graph edge_diff num_nodes
          next_graphs = Set.toList $ Set.map (\(u, f) -> updateNode graph u f) repls
          updateD u0 f0 u1 f1 = updateNode (updateNode graph u0 f0) u1 f1
          next_dgraphs = Set.toList $ Set.map (\((u0, f0), (u1, f1)) -> updateD u0 f0 u1 f1) drepls

vonKoch :: (Ord a) => EdgeGraphUF a Int -> [EdgeGraphUF a Int]
vonKoch graph = concat $ map (\g -> vonKoch' g max_edge_diff num_nodes) start_graphs
    where num_nodes = (length $ getUnfilled graph) + (length $ getFilled graph)
          max_edge_diff = num_nodes - 1
          first_node = NodeF 1
          start_graphs = Set.toList $ Set.map (\u -> updateNode graph u first_node) $ getUnfilled graph


checkSolnUF :: (Ord a, Show a) => EdgeGraphUF a Int -> EdgeGraphUF a Int -> Bool
checkSolnUF graph_unsolved graph_solved
    | num_unfilled > 0 = False
    | otherwise = (sort edges == [1..num_nodes-1]) && (iso adj_unsolved adj_solved)
    where num_unfilled = length $ getUnfilled graph_solved
          num_nodes = length $ getFilled graph_solved
          edges = map (\(NodeF v0, NodeF v1) -> abs (v0 - v1)) $ Set.toList graph_solved
          adj_unsolved = processGraph $ transEdgeToAdj $ Set.map (\(NodeU i, NodeU j) -> (i, j)) graph_unsolved
          adj_solved = processGraph $ transEdgeToAdj $ Set.map (\(NodeF i, NodeF j) -> (i, j)) graph_solved


-- isCaterpillar :: (Ord a, Integral b) => EdgeGraphUF a b -> Bool



main = do
    let m = Set.fromList [(NodeU 'a', NodeU 'b'), (NodeU 'b', NodeU 'c')] :: EdgeGraphUF Char Int
    let n = updateNode m (NodeU 'b') (NodeF 1)
    let g0 = Set.map (\(x,y) -> (NodeU x, NodeU y)) $ Set.fromList [('a', 'b'), ('c', 'b'), ('b', 'd'), ('d', 'e'), ('d', 'f')]
    print g0
    print $ getUnfilled g0
    print $ Set.map (\u -> updateNode g0 u (NodeF 1)) $ getUnfilled g0
    let g1 = updateNode g0 (NodeU 'b') (NodeF 1)
    print $ getReplacements g1 5 6

    print $ head $ vonKoch m
    print $ head $ vonKoch g0
    let g1 = Set.map (\(x,y) -> (NodeU x, NodeU y)) $ Set.fromList [('a', 'b'), ('c', 'b'), ('b', 'd'), ('d', 'e'),
                                                                    ('e', 'f'), ('e', 'g')]
    print $ head $ vonKoch g1
    print $ checkSolnUF g1 $ head $ vonKoch g1
    let g3 = Set.map (\(x,y) -> (NodeU x, NodeU y)) $ Set.fromList [('a','b'),('b','c'),('c','d'),('d','e'),('e','f'),('f','g'),
                                                                    ('g','h'),('h','i')]
    print $ head $ vonKoch g3

    let g4 = Set.map (\(x,y) -> (NodeU x, NodeU y)) $ Set.fromList $ map (\i -> ('a', i)) ['b'..'h']
    print $ head $ vonKoch g4
    let g2 = Set.map (\(x,y) -> (NodeU x, NodeU y)) $ Set.fromList [("1","6"),("2","6"),("3","6"),("4","6"),("5","6"),("5","7"),
                                                                    ("5","8"),("8","9"),("5","10"),("10","11"),("11","12"),
                                                                    ("11","13"),("13","14")] :: EdgeGraphUF String Int

    print $ head $ vonKoch g2

    print "hi"
