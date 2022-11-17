module Problem81 (paths) where

import qualified Data.Set as Set (Set, insert, member, fromList, filter, map)
import qualified Data.Map as Map ((!), member)

import Problem80 (AdjGraph)


paths' :: Ord a => a -> a -> AdjGraph a -> Set.Set a -> [[a]]
paths' src dst graph visited
    | src == dst = [[src]]
    | otherwise = map (\x -> (src:x)) $ concat $ Set.map (\s -> paths' s dst graph (Set.insert src visited)) to_visit
    where children = if Map.member src graph then
                         graph Map.! src
                     else
                         Set.fromList []
          to_visit = Set.filter (\x -> not $ Set.member x visited) children


paths :: Ord a => a -> a -> AdjGraph a -> [[a]]
paths src dst graph = paths' src dst graph (Set.fromList [])
