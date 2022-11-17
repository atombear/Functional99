module Problem71 (ipl) where

import Problem70 (MTree (MNode))


ipl' :: Int -> MTree a -> Int
ipl' depth (MNode _ node_list) = total
    where this_level_sum = (1 + depth) * (length node_list)
          lower_level_sum = sum (0:(map (ipl' (1+depth)) node_list))
          total = this_level_sum + lower_level_sum


ipl :: MTree a -> Int
ipl tree = ipl' 0 tree
