module Problem90 (queens) where


removeRow :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
removeRow (row,col) spots = [(i,j) | (i,j) <- spots, i /= row]


removeCol :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
removeCol (row,col) spots = [(i,j) | (i,j) <- spots, j/= col]


onDiag :: (Int,Int) -> (Int,Int) -> Bool
onDiag (i0,j0) (i1,j1) = (i0-i1) == (j0-j1) || (i0-i1) == (j1-j0)


removeDiag :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
removeDiag spot spots = filter (not.onDiag spot) spots


queens' :: [(Int,Int)] -> Int -> [[Int]]
queens' spots last_row
    | length spots == 0 = [[]]
    | otherwise = ds_queens
    where row = last_row+1
          spots_to_remove = filter (\(i,j) -> i == row) spots
          getSpots sp = (removeDiag sp.removeCol sp. removeRow sp) spots
          ds_queens = concat $ map (\sp -> (map (\x -> ((snd sp):x))) $ queens' (getSpots sp) row) spots_to_remove

queens :: Int -> [[Int]]
queens side = filter (\x -> side==(length x)) $ queens' [(i,j) | i <- [1..side], j <- [1..side]] 0
