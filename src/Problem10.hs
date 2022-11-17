module Problem10 (encode) where

myTakeWhile :: Eq a => [a] -> [a] -> ([a], [a])
myTakeWhile [] cache = ([], cache)
myTakeWhile (x:xs) [] = myTakeWhile xs [x]
myTakeWhile word@(x:xs) cache
    | x == head cache = myTakeWhile xs (x:cache)
    | otherwise = (word, cache)


processWord :: Eq a => [a] -> [(Int, a)] -> [(Int, a)]
processWord [] cache = reverse cache
processWord word cache = processWord shorter_word ((length grabbed, head grabbed):cache)
    where (shorter_word, grabbed) = myTakeWhile word []


encode :: Eq a => [a] -> [(Int, a)]
encode chars = processWord chars []
