module Problem19 (rotate) where


remove :: [a] -> [a]
remove (x:xs) = xs


compound :: (a -> a) -> Int -> (a -> a)
compound f n = foldl (\x -> \y -> x . y) id [f | _ <- [1..n]]


make_pos :: Int -> Int -> Int
make_pos val plus
    | val >= 0 = val
    | otherwise = make_pos (val + plus) plus


rotate :: [a] -> Int -> [a]
rotate word num = (compound remove num_pos) (word ++ (take num_pos word))
    where num_pos = make_pos num (length word)
