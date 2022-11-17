module Problem11 (encode_modified, Degeneracy(Multiple, Single)) where

data Degeneracy num val = Multiple num val | Single val deriving (Show, Eq)

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


encode_modified :: Eq a => [a] -> [Degeneracy Int a]
encode_modified chars = map transform_tuple $ processWord chars []
    where transform_tuple (num, val) = if num == 1 then (Single val) else (Multiple num val)
