module Problem12 (decode_modified) where


import Problem11 (Degeneracy(Multiple, Single))


decode_modified :: Eq a => [Degeneracy Int a] -> [a]
decode_modified list = foldl (++) [] $ map transform_degen list
    where transform_degen (Single val) = (val:[])
          transform_degen (Multiple num val) = [val | _ <- [1..num]]
