module Problem69 (tree2ds, ds2tree) where

import Problem55 (Tree (Node, Nil), toList)


tree2ds :: Tree Char -> [Char]
tree2ds Nil = "."
tree2ds (Node v left right) = (v:(tree2ds left)++(tree2ds right))



-- a binary graph has the property that there is always one more null than node. consider a single
-- node (1) which has two (2) nulls. every further addition of a null results in the addition of a node
-- which carries itself two nulls, thus effectively adding one node and one null, keeping the original
-- relationship. this property is used to identify the left and right sub-trees by counting until the
-- number of nulls is one more than the number of counted nodes (non-nulls).
ds2tree :: String -> Tree Char
ds2tree "." = Nil
ds2tree (x:xs) = Node x left right
    where (_, loc) = foldl (\(cnt, loc) -> \(idx, c) -> if (cnt == 1 && loc == -1) then
                                                            (cnt, idx)
                                                        else if (c == '.') then
                                                            (cnt+1, loc)
                                                        else
                                                            (cnt-1, loc))
                     (0, -1)
                     (zip [0..] xs)
          num = loc
          left = ds2tree $ take num xs
          right = ds2tree $ reverse $ take ((length xs) - num) $ reverse xs
