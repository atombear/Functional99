module Problem13 (encodeDirect) where


import Problem11 (Degeneracy(Multiple, Single))

advDegen :: Degeneracy Int a -> Degeneracy Int a
advDegen (Single val) = Multiple 2 val
advDegen (Multiple num val) = Multiple (num+1) val

valDegen :: Degeneracy Int a -> a
valDegen (Single val) = val
valDegen (Multiple num val) = val


encodeDirectHelper :: Eq a => [a] -> [Degeneracy Int a] -> [Degeneracy Int a]
encodeDirectHelper [] cache = reverse cache
encodeDirectHelper (x:xs) [] = encodeDirectHelper xs [(Single x)]
encodeDirectHelper (x:xs) cache@(last_element:rest)
    | x == (valDegen $ last_element) = encodeDirectHelper xs ((advDegen $ last_element):rest)
    | otherwise = encodeDirectHelper xs ((Single x):cache)

encodeDirect :: Eq a => [a] -> [Degeneracy Int a]
encodeDirect word = encodeDirectHelper word []
