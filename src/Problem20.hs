module Problem20 (removeAt) where


removeAt :: Int -> [a] -> (a, [a])
removeAt idx word = (word !! (idx - 1), [c | (jdx, c) <- zip [1..] word, idx /= jdx])