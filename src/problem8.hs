module Problem8 (compress) where


compress :: (Foldable t, Eq a) => t a -> [a]
compress word = reverse $ foldl (\x -> \y -> (if (length x == 0) then y:x else (if (head x == y) then x else (y:x)))) [] word
