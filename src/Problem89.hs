module Problem89 (bipartite) where


import qualified Data.Set as Set (fromList, union, Set, member, map, foldl, insert, intersection, take, toList)
import qualified Data.Map as Map (keysSet, foldl, (!))

import Problem80 (AdjGraph)
import Problem83 (processGraph)


bipartite' :: Ord a => AdjGraph a -> a -> Int -> (Set.Set a, Set.Set a) -> (Set.Set a, Set.Set a)
bipartite' graph entry group_num (group0, group1)
    | entry `Set.member` group = (group0, group1)
    | otherwise = Set.foldl (\(sa0, sa1) -> \(s0, s1) -> (Set.union sa0 s0, Set.union sa1 s1)) (empty_set, empty_set) process_children
    where empty_set = Set.fromList []
          group = if (mod group_num 2) == 0 then group0 else group1
          children = graph Map.! entry
          new_group = Set.insert entry group
          new_groups = if (mod group_num 2) == 0 then (new_group, group1) else (group0, new_group)
          process_children = Set.map (\x -> bipartite' graph x (group_num+1) new_groups) children

bipartite :: Ord a => AdjGraph a -> Bool
bipartite graph = not (length (s0 `Set.intersection` s1) > 0)
    where empty_set = Set.fromList []
          pgraph = processGraph graph
          nodes = (Map.keysSet pgraph) `Set.union` (Map.foldl Set.union empty_set pgraph)
          (s0, s1) = bipartite' pgraph (head $ Set.toList $ Set.take 1 nodes) 0 (empty_set, empty_set)