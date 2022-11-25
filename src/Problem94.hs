module Problem94 (graphK) where

import Control.Monad.State (State, execState, get, put)
import qualified Data.Set as Set (Set, fromList, toList, union, map, insert, difference, filter, member)

import Problem80 (AdjGraph, EdgeGraph, transEdgeToAdj, transAdjToEdge)
import Problem83 (processGraph)
import Problem85 (iso, isoBrute)

data Edge a = Edge a a deriving (Show)
type KGraph a = Set.Set (Edge a)

instance Eq a => Eq (Edge a) where
    (==) (Edge x0 x1) (Edge y0 y1) = (((x0==y0) && (x1==y1)) || ((x0==y1) && (x1==y0)))
    (/=) e0 e1 = not (e0 == e1)


instance Ord a => Ord (Edge a) where
    compare (Edge x0 x1) (Edge y0 y1) = compare (min x0 x1, max x0 x1) (min y0 y1, max y0 y1)


perms :: Ord a => [a] -> Int -> Set.Set (Set.Set a)
perms [] _ = Set.fromList []
perms (arr@(x:xs)) m
    | (m == 0) || (m > length arr) = Set.fromList []
    | m == 1 = Set.map (\el -> Set.fromList [el]) $ Set.fromList arr
    | m == length arr = Set.fromList [Set.fromList arr]
    | otherwise = (Set.map (Set.insert x) (perms xs (m-1))) `Set.union` (perms xs m)


rmEl :: Eq a => a -> [a] -> [a]
rmEl el arr = [i | i <- arr, i /= el]


getNodes :: Ord a => KGraph a -> Set.Set a
getNodes graph = union $ Set.map (\(Edge x0 x1) -> Set.fromList [x0, x1]) graph
    where union = foldl Set.union (Set.fromList [])


getCons :: Ord a => a -> KGraph a -> Set.Set a
getCons el graph = Set.map get_node $ Set.filter (\(Edge x0 x1) -> x0 == el || x1 == el) graph
    where get_node (Edge x0 x1) = if x0 == el then x1 else x0


addEdgeSearch :: Ord a => [a] -> Int -> Int -> KGraph a -> State ([KGraph a], KGraph a) ()
addEdgeSearch nodes idx cons edges = do
    (cache, graph) <- get
    put (cache, Set.union graph edges)
    graphKState nodes (idx+1) cons
    (cache, graph) <- get
    put (cache, Set.difference graph edges)
    return ()
    

graphKState :: Ord a => [a] -> Int -> Int -> State ([KGraph a], KGraph a) ()
graphKState nodes idx cons = let
                                 num_nodes = length nodes
                                 node = nodes !! idx
                                 rest = rmEl node nodes
                             in do
    (cache, graph) <- get
    if (length graph) == (div (num_nodes * cons) 2) then do
        put ((graph:cache), graph)
        return ()
    else if idx == num_nodes then
        return ()
    else do
        let connected = getCons node graph
            num_edges = length $ Set.filter (\(Edge x0 x1) -> (x0 == node) || (x1 == node)) graph
            available = [n | n <- nodes, (not $ Set.member n connected) &&
                                         (n /= node) &&
                                         ((length $ getCons n graph) < cons)]
            children = perms available (cons - num_edges)
            edges_set = Set.map (Set.map (Edge node)) children
            -- new_edges_set = Set.filter (\x -> cons == length (Set.difference x graph)) edges_set
        mapM_ (addEdgeSearch nodes idx cons) edges_set
        return ()


removeIso :: Ord a => [KGraph a] -> [AdjGraph a]
removeIso graphs = unique_graphs
    where edge_graphs = map (Set.map (\(Edge x0 x1) -> (x0, x1))) graphs
          adj_graphs = map (processGraph.transEdgeToAdj) edge_graphs
          unique_graphs = foldl (\accum -> \g ->
                                    if any (isoBrute g) accum then
                                        accum
                                    else
                                        (g:accum))
                                []
                                adj_graphs


graphK :: Int -> Int -> [AdjGraph Int]
graphK nodes rank = removeIso $ fst $ execState (graphKState [0..(nodes-1)] 0 rank) ([], Set.fromList [])


main = do
    let x = Set.fromList [(Edge 4 2)]
    print $ head $ Set.toList x
    let g0 = Set.fromList [Edge 0 3, Edge 0 4, Edge 0 5, Edge 1 3, Edge 1 4, Edge 1 5, Edge 2 3, Edge 2 4, Edge 2 5]
    let g1 = Set.fromList [Edge 0 3, Edge 0 4, Edge 0 5, Edge 2 1, Edge 2 3, Edge 2 4, Edge 5 1, Edge 5 3, Edge 4 1]
    print $ removeIso $ fst $ execState (graphKState [0..5] 0 3) ([], Set.fromList [])
