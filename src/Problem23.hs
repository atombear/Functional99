module Problem23 (rnd_select) where

import System.Random (mkStdGen, randomR, randomRIO)


getRandomList :: Int -> Int -> Int -> [Int] -> IO [Int]
getRandomList size max_val seed cache
    | size == length cache = return cache
    | otherwise = do
        let g = mkStdGen seed
        let val = fst $ (randomR (0, max_val-1)) g
        if (elem val cache) then
            getRandomList size max_val (seed+1) cache
        else
            getRandomList size max_val (seed+1) (val:cache)


rnd_select :: [c] -> Int -> IO [c]
rnd_select word size = do
    seed <- randomRIO (1, 1000000)
    idxs <- getRandomList size (length word) seed []
    return [word !! idx | idx <- idxs]



main = do
    getRandomList 5 10 1 [] >>= print

    getRandomList 5 10 2 [] >>= print
    getRandomList 5 10 3 [] >>= print
    getRandomList 5 10 4 [] >>= print
    getRandomList 5 10 5 [] >>= print
    getRandomList 5 10 6 [] >>= print
    getRandomList 5 10 7 [] >>= print
    getRandomList 5 10 8 [] >>= print


    getRandomList 3 10 943 [] >>= print
    getRandomList 3 10 3239 [] >>= print
    getRandomList 3 10 5234952 [] >>= print
    getRandomList 3 10 423952 [] >>= print
    getRandomList 3 10 259 [] >>= print
