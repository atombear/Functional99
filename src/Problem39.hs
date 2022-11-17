module Problem39 (primesR) where


setFalse :: Int -> [Bool] -> [Bool]
setFalse idx list
    | idx >= length list = list
    | otherwise = xs ++ (False:ys)
    where (xs, _:ys) = splitAt idx list


setMultiplesFalse :: Int -> [Bool] -> [Bool]
setMultiplesFalse mul list = foldl (.) id [setFalse idx | idx <- (fmap (mul*) [2..div (length list) mul])] $ list


getIdxs :: [Bool] -> [Int]
getIdxs list = map fst $ filter (\(idx, bool) -> bool) $ zip [0..] list


primesR :: Int -> Int -> [Int]
primesR start stop = filter (\x -> x >= start) $ getIdxs $ set_all ([False, False] ++ [True | _ <- [0..stop-2]])
    where set_all list = foldl (.) id [setMultiplesFalse mul | mul <- [2..div (length list) 2]] $ list
