module Problem15 (repli) where


repli :: [a] -> Int -> [a]
repli list n = foldl (++) [] $ map (\x -> [x | _ <- [1..n]]) list
