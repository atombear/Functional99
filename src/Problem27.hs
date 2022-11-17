module Problem27 (group) where


import Problem26 (combinations)


group :: Eq a => [Int] -> [a] -> [[[a]]]
group group_nums vals
    | length group_nums == 1 = [[vals]]
    | otherwise = foldl (++) [] (map (\l -> [l:m | m <- group (tail group_nums) (rest l)]) leads)
    where leads = combinations (head group_nums) vals
          rest l = [v | v <- vals, not (elem v l)]