module Problem88 (connectedComponents) where


import qualified Data.Map as Map ((!), keysSet, Map, foldl)
import qualified Data.Set as Set (Set, difference, union, toList, fromList, member, insert)
import Control.Monad.State (State, execState, get, put)

import Problem80 (AdjGraph)


cv_state :: Ord a => AdjGraph a -> a -> State (Set.Set a) ()
cv_state graph entry = do
    visited <- get
    let children = graph Map.! entry
        next_nodes = children `Set.difference` visited
    put $ visited `Set.union` next_nodes
    mapM (cv_state graph) (Set.toList next_nodes)
    return ()


collectVisited :: Ord a => AdjGraph a -> a -> Set.Set a
collectVisited graph entry = execState (cv_state graph entry) (Set.fromList [])


connectedComponents :: Ord a => AdjGraph a -> [[a]]
connectedComponents graph = node_sets
    where empty_set = Set.fromList []
          nodes = Set.toList $ (Map.keysSet graph) `Set.union` (Map.foldl Set.union empty_set graph)
          node_sets = (map Set.toList) $ fst $ (foldl (\(accum, visited) -> \node ->
                                                      if (node `Set.member` visited) then
                                                          (accum, visited)
                                                      else
                                                          let vnodes = collectVisited graph node
                                                          in ((vnodes:accum), Set.union vnodes visited)))
                                               ([], empty_set)
                                               nodes
