module Problem21 (insertAt) where

insertAt :: Eq a => a -> [a] -> Int -> [a]
insertAt c word idx = foldl (\x -> \(jdx, y) -> if (jdx < idx) then (y:x) else (reverse $ (y:(reverse x)))) (c:[]) (zip [1..] word)
