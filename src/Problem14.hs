module Problem14 (dupli) where


dupli :: [a] -> [a]
dupli list = foldl (++) [] $ map (\x -> [x, x]) list
