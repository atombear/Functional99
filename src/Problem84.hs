module Problem84 (prim) where

import qualified Data.Set as Set (fromList, toList)
import qualified Data.Map as Map ((!))

import Problem80 (transEdgeToAdj, AdjGraph)
import Problem83 (processGraph)


getUnique :: Eq a => [a] -> [a]
getUnique list = foldl (\accum -> \el -> if elem el accum then accum else (el:accum)) [] list


findIdx :: Eq a => a -> [a] -> Int
findIdx el (x:xs)
    | x == el = 0
    | otherwise = 1 + (findIdx el xs)


listReplace :: [a] -> Int -> a -> [a]
listReplace list idx val = [if jdx == idx then val else el | (jdx, el) <- zip [0..] list]


primBrute :: Ord a => AdjGraph a -> [((a,a),Int)] -> [a] -> [a] -> [(a,a)] -> [(a,a)]
primBrute graph edges_weights nodes tree edges
    | length tree == length nodes = edges
    | otherwise = primBrute graph edges_weights nodes new_tree new_edges
    where a_node = head nodes
          available_nodes = filter (\n -> not $ elem n tree) nodes 
          available_edges = filter (\((i,j),w) -> (elem i tree && elem j available_nodes) || (elem j tree && elem i available_nodes)) edges_weights
          min_edge = foldl (\((i0,j0),w0) -> \((i1,j1),w1) -> if w0 < w1 then ((i0,j0),w0) else ((i1,j1),w1)) ((a_node,a_node),1000) available_edges
          ((ni, nj), _) = min_edge
          new_edges = ((ni,nj):edges)
          new_tree = if elem ni tree then (nj:tree) else (ni:tree)


primElegant :: Ord a => AdjGraph a -> [((a,a),Int)] -> [a] -> [Int] -> [Int] -> [(a, a)] -> [(a, a)]
primElegant graph edges_weights nodes nodes_idx nodes_vals nodes_edges
    | length nodes_idx == length nodes = nodes_edges
    | otherwise = primElegant graph edges_weights nodes new_nodes_idx new_nodes_vals new_nodes_edges
    where (_, nn_idx) = minimum [(v, idx) | (v, idx) <- zip nodes_vals [0..], not $ elem idx nodes_idx]
          new_nodes_idx = (nn_idx:nodes_idx)
          nn = nodes !! nn_idx
          children = filter (\x -> if elem (findIdx x nodes) nodes_idx then False else True) $ Set.toList $ graph Map.! nn
          c_idx = map (\x -> findIdx x nodes) children
          c_weights = map (\c -> (snd.head) $ filter (\((i,j),w) -> (i,j) == (c,nn) || (i,j) == (nn,c)) edges_weights) children
          (new_nodes_vals, new_nodes_edges) = foldl (\(nv, ne) -> \(c_i, c_w) -> if (nodes_vals !! c_i) > c_w then (listReplace nv c_i c_w, listReplace ne c_i (nn,nodes !! c_i)) else (nv, ne)) (nodes_vals, nodes_edges) $ zip c_idx c_weights


prim :: Ord a => [((a, a),Int)] -> AdjGraph a
prim edges_weights = processGraph $ transEdgeToAdj $ Set.fromList mst
    where edges = map fst edges_weights
          nodes = getUnique $ (map fst edges) ++ (map snd edges)
          graph = processGraph $ transEdgeToAdj $ Set.fromList edges
--          mst = primBrute graph edges_weights nodes [head nodes] []
          mst = primElegant graph edges_weights nodes [] (0:[100 | _ <- [2..(length nodes)]]) [(head nodes, head nodes) | _ <- nodes]
