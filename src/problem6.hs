module Problem6 (isPalindrome) where

isPalindrome :: Eq a => [a] -> Bool
isPalindrome word
    | length word == 0 = True
    | length word == 1 = True
    | head word == last word = isPalindrome $ (tail . init) word
    | otherwise = False