module Problem9 (pack) where

myTakeWhile :: Eq a => [a] -> [a] -> ([a], [a])
myTakeWhile [] cache = ([], cache)
myTakeWhile (x:xs) [] = myTakeWhile xs [x]
myTakeWhile word@(x:xs) cache
    | x == head cache = myTakeWhile xs (x:cache)
    | otherwise = (word, cache)


processWord :: Eq a => [a] -> [[a]] -> [[a]]
processWord [] cache = reverse cache
processWord word cache = processWord shorter_word (grabbed:cache)
    where (shorter_word, grabbed) = myTakeWhile word []


pack :: Eq a => [a] -> [[a]]
pack chars = processWord chars []
