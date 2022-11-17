module Problem87 (depthFirst) where

import qualified Data.Map as Map ((!), toList)
import qualified Data.Set as Set (toList)

import Problem80 (AdjGraph)


removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates list = reverse $ foldl (\accum -> \el -> if elem el accum then accum else (el:accum)) [] list

depthFirst :: Ord a => AdjGraph a -> a -> [a]
depthFirst graph entry
    | length children == 0 = [entry]
    | otherwise = removeDuplicates $ [entry] ++ (concat $ map (depthFirst graph) children)
    where children = Set.toList $ graph Map.! entry
