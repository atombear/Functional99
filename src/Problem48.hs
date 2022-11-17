module Problem48 (tablen) where

listTrueFalse :: Int -> [[Bool]] -> [[Bool]]
listTrueFalse 0 cache = cache
listTrueFalse n cache = listTrueFalse (n-1) ([(True:x) | x <- cache] ++ [(False:x) | x <- cache])


printBool :: Bool -> String
printBool x = if x then "true" else "fail"

printBoolList :: [Bool] -> IO ()
printBoolList [] = return ()
printBoolList (x:xs) = do
    putStr $ if x then "true " else "fail "
    printBoolList xs


tablenHelper :: [[Bool]] -> ([Bool] -> Bool) -> IO ()
tablenHelper [] _ = return ()
tablenHelper (x:xs) f = do
    printBoolList x
    putStr $ printBool $ f x
    putStr "\n"
    tablenHelper xs f


tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = tablenHelper (listTrueFalse n [[]]) f
