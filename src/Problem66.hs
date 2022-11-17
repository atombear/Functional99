module Problem66 (layoutTight) where

import Problem55 (Tree (Node, Nil), toList)


compound :: (a -> a) -> Int -> (a -> a)
compound f n = foldl (\x -> \y -> x.y) id [f | _ <- [1..n]]


stripRight :: Eq a => [a] -> a -> [a]
stripRight list val = (compound init num_to_drop) list
    where num_to_drop = length.(takeWhile (\x -> x == val)) $ reverse list


stripLeft :: Eq a => [a] -> a -> [a]
stripLeft list val = (compound tail num_to_drop) list
    where num_to_drop = length.(takeWhile (\x -> x == val)) $ list


getLeafs :: Eq a => [Maybe a] -> [Int]
getLeafs list = reverse $ foldl (\xs -> \(idx, v) -> if is_childless idx then
                                                         (idx:xs)
                                                     else
                                                         xs)
                          []
                          (zip [0..] list)
    where is_childless parent = ((2*parent+1 >= length list || list !! (2*parent+1) == Nothing) &&
                                 (2*parent+2 >= length list || list !! (2*parent+2) == Nothing))


-- layoutTight :: Eq a => Tree a -> [Int]
-- layoutTight tree = children
--     where list = stripRight (toList tree) Nothing
--           children = getLeafs $ list


layoutTightProc :: Eq a => Tree a -> Tree (a, ([Int], [Int]))
layoutTightProc Nil = Nil
layoutTightProc (Node val left right) = Node (val, (left_offset, right_offset))
                                             (if left == Nil then Nil else process_left)
                                             (if right == Nil then Nil else process_right)
    where left_offset = if left == Nil then
                            [0]
                        else
                            (-offset:[i-offset | i <- ll])
          right_offset = if right == Nil then
                             [0]
                         else
                             (offset:[i+offset | i <- rr])
          closest = abs $ minimum [(i-j) | (i,j) <- zip (if right == Nil then [0] else rl)
                                                        (if left == Nil then [0] else lr)]
          offset = 1+(div closest 2)
          process_left = layoutTightProc left
          process_right = layoutTightProc right
          Node (_, (ll, lr)) _ _ = process_left
          Node (_, (rl, rr)) _ _ = process_right


layoutTightX :: (Eq a) => Tree (a, ([Int], [Int])) -> Int -> Int -> Tree (a, (Int, Int))
layoutTightX Nil _ _ = Nil
layoutTightX (Node (val, (left_offset, right_offset)) left right) loc depth = Node (val, (loc, depth)) new_left new_right
    where new_left = layoutTightX left (loc + head left_offset) (depth+1)
          new_right = layoutTightX right (loc + head right_offset) (depth+1)


layoutTight :: (Eq a) => Tree a -> Tree (a, (Int, Int))
layoutTight Nil = Nil
layoutTight tree = layoutTightX x_proc loc 1
    where x_proc = layoutTightProc tree
          Node (_, (left_offset, right_offset)) _ _ = x_proc
          loc = 1 - (minimum left_offset)
