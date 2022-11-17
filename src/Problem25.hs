module Problem25 (rnd_permu) where

import System.Random (randomRIO)


rmElement :: Eq a => [a] -> Int -> [a]
rmElement list idx = [el | (jdx, el) <- zip [0..] list, jdx /= idx]


getRemovalList :: Int -> IO [Int]
getRemovalList 1 = return [0]
getRemovalList len = do
    v <- randomRIO (0, len-1)
    vs <- getRemovalList (len-1)
    return (v:vs)


applyOrdering :: Eq a => [a] -> [Int] -> [a] -> [a]
applyOrdering [] _ cache = cache
applyOrdering _ [] cache = cache
applyOrdering list (x:xs) cache = applyOrdering rm_list xs (el:cache)
    where el = list !! x
          rm_list = rmElement list x


rnd_permu :: Eq a => [a] -> IO [a]
rnd_permu list = do
    rm_ordering <- getRemovalList $ length list
    return $ applyOrdering list rm_ordering []
