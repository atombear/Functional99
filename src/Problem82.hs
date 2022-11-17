module Problem82 (graph_cycle) where

import qualified Data.Map as Map ((!), member)
import qualified Data.Set as Set (map)

import Problem80 (AdjGraph)
import Problem81 (paths)


graph_cycle :: Ord a => a -> AdjGraph a -> [[a]]
graph_cycle node graph
    | not $ Map.member node graph = []
    | otherwise = map (\x -> node:x) paths_from_children
    where children = graph Map.! node
          paths_from_children = concat $ Set.map (\x -> paths x node graph) children
