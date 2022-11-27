module Problem98 (nonogram, Col (BlockHold)) where

-- encode the column state machine in this type.
-- a column can accumulate values and will recognize boundaries.
data Col a = BlockHold [a] | BlockAccum a [a] | Zero | Reject deriving (Show, Eq)


-- remove the first incidence of an element from a list.
rmEl :: Eq a => a -> [a] -> [a]
rmEl _ [] = []
rmEl v (x:xs) = if x == v then xs else (x:(rmEl v xs))


-- the rules to advance the state machine
advance :: Int -> Col Int -> Col Int
advance _ Reject = Reject

advance 0 Zero = Zero
advance _ Zero = Reject

advance 0 (BlockHold v) = BlockHold v
advance n (BlockHold v) = if all ((>)n) v then
                              Reject
                          else
                              (BlockAccum n v)

advance 0 (BlockAccum accum v) = if any ((==)accum) v then
                                     if length v == 1 then
                                         Zero
                                     else
                                         BlockHold (rmEl accum v)
                                 else
                                     Reject

advance n (BlockAccum accum v) = if all ((>)(n+accum)) v then
                                     Reject
                                 else
                                     BlockAccum (n+accum) v


zeros :: Int -> [Int]
zeros n = [0 | _ <- [1..n]]

ones :: Int -> [Int]
ones n = [1 | _ <- [1..n]]


inBlocks :: Int -> Int -> [[Int]]
inBlocks total num_blocks
    | num_blocks == 1 = [[total]]
    | otherwise = concat [map (n:) $ inBlocks (total - n) (num_blocks - 1) | n <- [0..total]]


zipper :: [a] -> [a] -> [a]
zipper list0 list1 = reverse (last list0:(foldl (\accum -> \(el0, el1) -> (el1:el0:accum)) [] $ zip list0 list1))


arrToZeroOne :: [Int] -> [Int]
arrToZeroOne arr = concat [if even n then zeros block else ones block | (n, block) <- zip [0..] arr]


arrangements :: [Int] -> Int -> [[Int]]
arrangements [] _ = []
arrangements blocks total = map arrToZeroOne $ map (\x -> zipper x blocks) zero_dist
    where zs = total - (sum blocks)
          zero_dist = filter (\x -> not $ elem 0 $ (init.tail) x) $ inBlocks zs ((length blocks) + 1)


-- a column is in a terminal state if it is `Zero`, ie it has exhausted all its blocks and has seen a boundary, or
-- `BlockAccum v [v]` in which case it has exhausted its blocks and the boundary is assumed to be the end of the board.
endstateCol :: Col Int -> Bool
endstateCol Zero = True
endstateCol (BlockAccum va [vb]) = va == vb
endstateCol _ = False


nonogram :: [[Int]] -> Int -> [[Int]] -> [Col Int] -> [[Int]]
nonogram board _ [] cols = if all endstateCol cols then board else []
nonogram board size (r:rs) cols = if length step_all > 0 then (head step_all) else []
    where arrs = arrangements r size
          advs = [[advance v c | (v, c) <- zip arr cols] | arr <- arrs]
          filter_advs = filter (not.(elem Reject).snd) $ zip arrs advs
          step_all = filter (\x -> length x > 0) [nonogram (arr:board) size rs adv | (arr, adv) <- filter_advs]


main = do
    let rows = [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]]
    let cols = map BlockHold [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]
    print $ reverse $ nonogram [] 8 rows cols
