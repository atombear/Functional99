module Problem91 (findTour, findLoopTour, checkBoard, findZero) where

import GHC.Ix (Ix)

import Control.Monad.State (execStateT, StateT, get, put, lift)
import Control.Monad (unless)
import Control.Monad.ST (runST, ST)
import Control.Monad.Extra ((&&^), (||^))

import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef)
import Data.Array.ST (newArray, readArray, writeArray, STUArray, getBounds, STArray)
import Data.Array.Base (getNumElements, unsafeRead)
import Data.MemoTrie (memo2)
import Data.List (sort)

import Debug.Trace (trace)

type Board s = STUArray s (Int, Int) Int
data BoardConfig = BoardConfig { startBC :: (Int, Int), sizeBC :: Int, findloopBC :: Bool }


-- search over these relative locations
rd :: [Int]
rd = [2,1,-1,-2,-2,-1,1,2]

cd :: [Int]
cd = [1,2,2,1,-1,-2,-2,-1]


-- unless, but with a monadic context
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mbool statement = mbool >>= (\bool -> unless bool statement)



anyM :: (Monad m, Eq e) => (i -> m e) -> e -> [i] -> m Bool
anyM k el [] = return False
anyM k el (x:xs) = (k x) >>= \r -> if r == el then (return True) else (anyM k el xs)


-- `False` if any element of the board is `-1`
isSolved :: Board s -> ST s Bool
isSolved arr = do
    ((r0,c0),(r1,c1)) <- getBounds arr
    anyM (\i -> readArray arr i) (-1) [(idx, jdx) | idx <- [r0..r1], jdx <- [c0..c1]]


findElemM :: Ix i => Int -> STUArray s i Int -> Int -> Int -> ST s Bool
findElemM e arr idx len
    | idx < len = do
        e' <- unsafeRead arr idx
        if e == e' then
            return True
        else
            findElemM e arr (idx+1) len
    | otherwise = return False

isSolved' :: Ix i => STUArray s i Int -> ST s Bool
isSolved' arr = do
    n <- getNumElements arr
    fmap not $ findElemM (-1) arr 0 n


reachable :: Int -> (Int, Int) -> [(Int, Int)]
reachable = memo2 $ (\size -> \(idx,jdx) -> filter (\(i,j) -> (0<=i) && (i<size) && (0<=j) && (j<size)) [(idx+r, jdx+c) | (r, c) <- zip rd cd])


-- given the board and a location, find the locations to where the knight can travel
getNextLocs :: BoardConfig -> Board s -> (Int, Int) -> ST s [(Int, Int)]
getNextLocs bc board (idx, jdx) = let locs = reachable (sizeBC bc) (idx, jdx) in do
    values <- mapM (\i -> readArray board i) locs
    return [p | (p, v) <- zip locs values, v == -1]


isLoopTour :: BoardConfig -> StateT (Board s) (ST s) Bool
isLoopTour bc = let
                      size = sizeBC bc
                      start = startBC bc
                      ridx = (reachable size start)
                      final_val = (size^2) - 1
                  in do
    board <- get
    lift $ anyM (\i -> readArray board i) final_val ridx


solveFromPosition :: [(Int, Int)] -> Int -> BoardConfig -> StateT (Board s) (ST s) Bool
solveFromPosition [] _ _ = return False
solveFromPosition (x:xs) step bc = let
                                size = sizeBC bc
                                nofindloop = not $ findloopBC bc
                            in do
    solved <- (return $ step == (size^2) - 1) &&^ ((return nofindloop) ||^ (isLoopTour bc))
    if solved then
        return True
    else do
        solved <- knightFrom x (step+1) bc
        if solved then
            return True
        else do
            solved <- solveFromPosition xs step bc
            return solved


-- find a tour if the knight starts at `(idx,jdx)` traversing the board held in state
-- `step` is the enumerated knight that is being placed
knightFrom :: (Int, Int) -> Int -> BoardConfig -> StateT (Board s) (ST s) Bool
knightFrom (idx, jdx) step bc = let
                                    size = sizeBC bc
                                    nofindloop = not $ findloopBC bc
                                in do

    -- get the board from the state
    board <- get

    -- update the board to place knight `step` at location `(idx, jdx)` 
    lift $ writeArray board (idx, jdx) step

    -- if the board is solved, return
    solved <- (return $ step == (size^2) - 1) &&^ ((return nofindloop) ||^ (isLoopTour bc))
    if solved then
        return True
    else do
        -- get the available locations
        next_locs <- lift $ getNextLocs bc board (idx, jdx)

        -- Warnsdorff
        num_adj <- lift $ mapM (\p -> (fmap length) $ getNextLocs bc board p) next_locs
        let locsW = map snd $ sort $ zip num_adj next_locs
        
        -- act on the available locations until (?) the board is solved
        solved <- solveFromPosition locsW step bc

        -- backtrack if the board is not solved
        if solved then
            return True
        else do
            lift $ writeArray board (idx, jdx) (-1)
            return False
    

readInto2DList :: ST s (Board s) -> ST s [[Int]]
readInto2DList st_arr = do
    arr <- st_arr
    ((x0, y0), (x1, y1)) <- getBounds arr
    mapM (\j -> mapM (\i -> readArray arr (i, j)) [x0..x1]) [y0..y1]


findTour :: Int -> [[Int]]
findTour size = runST $ let
                            start = (0, 0)
                            bc = BoardConfig { sizeBC = size, startBC = start, findloopBC = False }
                        in do
    brd <- newArray ((0, 0), (size-1, size-1)) (-1) :: ST s (Board s)
    readInto2DList $ execStateT (knightFrom start 0 bc) brd


findLoopTour :: Int -> (Int, Int) -> [[Int]]
findLoopTour size start = runST $ let
                                      bc = BoardConfig { sizeBC = size, startBC = start, findloopBC = True }
                                  in do
    brd <- newArray ((0, 0), (size-1, size-1)) (-1) :: ST s (Board s)
    readInto2DList $ execStateT (knightFrom start 0 bc) brd


-- find the starting location
findZero :: [[Int]] -> (Int, Int)
findZero board = (idx, jdx)
    where (idx, row) = head $ filter (\(i, r) -> elem 0 r) $ zip [0..] board
          (jdx, col) = head $ filter (\(j, c) -> c == 0) $ zip [0..] row


-- assess whether a board is a solution
checkBoard :: [[Int]] -> (Int, Int) -> Bool
checkBoard board (idx, jdx) = result
    where size = length board
          val = (board !! idx) !! jdx
          r = reachable size (idx, jdx)
          next_loc = head $ filter (\(i, j) -> ((board !! i) !! j) == val+1) r
          result = if val == (size^2) - 1 then
                       True
                   else if length r == 0 then
                       False
                   else
                       checkBoard board next_loc




-- -- -- -- -- -- --
-- scratchwork
updateVec :: ST s (STRef s [Int]) -> ST s [Int]
updateVec st_vec = do
    vec <- st_vec
    modifySTRef vec (map (\x -> x+1))
    readSTRef vec


addOne :: ST s (STArray s Int Int) -> Int -> ST s (STArray s Int Int)
addOne st_vec idx = do
    vec <- st_vec
    val <- readArray vec idx
    writeArray vec idx (val + 1)
    return vec


readIntoList :: ST s (STArray s Int a) -> ST s [a]
readIntoList st_vec = do
    vec <- st_vec
    (low, high) <- getBounds vec
    mapM (\x -> readArray vec x) [low..high]
-- end scratchwork
-- -- -- -- -- -- --


main = do
    let x = newSTRef [1,2,3]
        y = updateVec x
        z = runST y
    print z

    let x0 = (newArray (0, 9) 0) :: ST s (STArray s Int Int)
        addOneToZero = \x -> addOne x 0
        z = runST $ ((addOneToZero.addOneToZero.addOneToZero) x0 >>= (\x -> readArray x 0))
        zl = runST $ readIntoList (addOneToZero x0)
    print $ z
    print $ zl
    let x2 = newArray ((0, 0), (3, 5)) 5 :: ST s (Board s)
        x2l = runST $ readInto2DList x2
    print x2l
    print $ findTour 5
    print "hello"
