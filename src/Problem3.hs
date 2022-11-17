module Problem3 (elementAt) where

elementAt :: [a] -> Int -> a
elementAt list n
    | n > length list = error ("index " ++ (show n) ++ " out of range!")
    | n == length list = last list
    | otherwise = elementAt (init list) n

