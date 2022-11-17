module Problem72 (bottom_up) where


import Problem70 (MTree (MNode))


bottom_up' :: MTree Char -> String
bottom_up' (MNode val node_list) = (val:(foldl (++) "" $ map bottom_up' $ reverse node_list))


bottom_up :: MTree Char -> String
bottom_up tree = reverse $ bottom_up' tree