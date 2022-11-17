module Problem24 (diff_select) where

import System.Random (randomRIO)

diff_select :: Int -> Int -> IO [Int]
diff_select 0 max_val = return []
diff_select num max_val = do
    v <- randomRIO (1, max_val)
    vs <- diff_select (num-1) max_val
    return (v:vs)