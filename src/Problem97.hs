module Problem97 (loadSolve, fullCheck, arrFromIOArray) where


import System.Directory (getCurrentDirectory)
import Data.Array.IO (newArray_, readArray, writeArray, IOUArray)
import Data.Array.Base (getNumElements, unsafeRead, unsafeWrite, getNumElements)
import Control.Monad (filterM, Monad)
import Data.List (sort)


rng :: [Int]
rng = [0..8]

anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM f [] = return False
anyM f (x:xs) = do
    b <- f x
    if b then
        return True
    else
        anyM f xs


type Board = IOUArray (Int, Int) Int


sud_locs :: [(Int, Int)]
sud_locs = [(idx, jdx) | idx <- rng, jdx <- rng]


getBoxLocs :: (Int, Int) -> [(Int, Int)]
getBoxLocs (idx, jdx) = [(i, j) | i <- [min_row..min_row+2], j <- [min_col..min_col+2]]
    where min_row = 3 * (div idx 3)
          min_col = 3 * (div jdx 3)


readLocalFile :: String -> IO String
readLocalFile fname = do
    dir <- getCurrentDirectory
    readFile $ pathJoin dir fname


pathJoin :: String -> String -> String
pathJoin path0 path1 = if (last path0 == '/') && (head path1 == '/') then
                           path0 ++ (tail path1)
                       else if (last path0 =='/') || (head path1 == '/') then
                           path0 ++ path1
                       else    
                           path0 ++ ('/':path1)


splitAtChar :: String -> Char -> [String]
splitAtChar word char = clean $ foldl (\accum -> \el ->
                                          if el == char then
                                              ("":accum)
                                          else
                                              ((el:(head accum)):(tail accum)))
                                      [""]
                                      word
    where clean = (\arr -> filter (\x -> (length x) > 0) $ reverse $ map reverse arr)


stringToArray :: String -> [[Int]]
stringToArray mat_string = [[read n :: Int | n <- splitAtChar nums ','] | nums <- splitAtChar mat_string '\n']


loadIntoIOArray :: [[Int]] -> IO Board
loadIntoIOArray arr = do
    ioarr <- newArray_ ((0,0), (8,8))
    mapM (\(idx, jdx) -> writeArray ioarr (idx, jdx) ((arr !! idx) !! jdx)) sud_locs
    return ioarr


arrFromIOArray :: Board -> IO [[Int]]
arrFromIOArray arr = mapM (\idx -> (mapM (\jdx -> readArray arr (idx, jdx)) rng)) rng


getEmptyLocs :: Board -> IO [(Int, Int)]
getEmptyLocs arr = do
    vals <- mapM (\i -> readArray arr i) sud_locs
    return $ map fst $ filter (\((idx, jdx), v) -> v == -1) $ zip sud_locs vals


getRowVals :: Board -> Int -> IO [Int]
getRowVals arr row = do
    vals <- mapM (\jdx -> readArray arr (row, jdx)) rng
    return $ filter ((/=)(-1)) vals


getColVals :: Board -> Int -> IO [Int]
getColVals arr col = do
    vals <- mapM (\idx -> readArray arr (idx, col)) rng
    return $ filter ((/=)(-1)) vals


getBoxVals :: Board -> (Int, Int) -> IO [Int]
getBoxVals arr pt = do
    vals <- mapM (\i -> readArray arr i) $ getBoxLocs pt
    return $ filter ((/=)(-1)) vals


getVals :: Board -> (Int, Int) -> IO [Int]
getVals arr (idx, jdx) = do
    row_vals <- getRowVals arr idx
    col_vals <- getColVals arr jdx
    box_vals <- getBoxVals arr (idx, jdx)
    return [v | v <- [1..9], not (elem v (row_vals ++ col_vals ++ box_vals))]


firstFill :: Board -> IO Board
firstFill arr = do
    empty_locs <- getEmptyLocs arr
    loc_vals <- mapM (\i -> getVals arr i) empty_locs
    let single_locs = filter (\(x, y) -> length x == 1) $ zip loc_vals empty_locs
    mapM (\([v], i) -> writeArray arr i v) single_locs
    return arr


printArr :: Show a => [a] -> IO ()
printArr [] = return ()
printArr (x:xs) = do
    print x
    printArr xs
    return ()


isSolved :: Board -> IO Bool
isSolved board = do
    num <- getNumElements board
    fmap not $ anyM (\i -> (unsafeRead board i) >>= (\x -> return (x == -1))) [0..(num-1)]


updateRun :: Board -> (Int, Int) -> Int -> IO Board
updateRun board loc val = do
    solved <- isSolved board
    if solved then
        return board
    else do
        writeArray board loc val
        fullSolve board
        solved <- isSolved board
        if solved then
            return board
        else do
            writeArray board loc (-1)
            return board


fullSolve :: Board -> IO Board
fullSolve board = do
    solved <- isSolved board
    if solved then
        return board
    else do
        loc <- fmap head $ getEmptyLocs board
        loc_vals <- getVals board loc
        mapM (\x -> updateRun board loc x) loc_vals
        return board

solve :: Board -> IO Board
solve board = do
    first_solve <- firstFill board
    fullSolve first_solve


fullCheck :: Board -> IO Bool
fullCheck board = let
                      center_locs = [(idx,jdx) | idx <- [1,4,7], jdx <- [1,4,7]]
                  in do
    check_boxs <- fmap (all id) $ mapM checkBox center_locs
    check_rows <- fmap (all id) $ mapM checkRow rng
    check_cols <- fmap (all id) $ mapM checkCol rng
    return (check_boxs && check_rows && check_cols)
    where check_list = (\l -> (sort l) == [1..9])
          checkBox loc = fmap check_list $ mapM (readArray board) $ getBoxLocs loc :: IO Bool
          checkRow row = fmap check_list $ mapM (readArray board) $ [(row, jdx) | jdx <- rng] :: IO Bool
          checkCol col = fmap check_list $ mapM (readArray board) $ [(idx, col) | idx <- rng] :: IO Bool


loadSolve :: String -> IO Board
loadSolve fname = (fmap stringToArray (readLocalFile fname)) >>= loadIntoIOArray >>= solve


main = do
    mat_str <- readLocalFile "sudoku0.txt"
    print $ stringToArray mat_str
    (loadIntoIOArray $ stringToArray mat_str) >>= arrFromIOArray >>= print
    x <- loadIntoIOArray $ stringToArray mat_str
    y <- getNumElements x
    z <- mapM (\i -> unsafeRead x i) [0..(y-1)]
    print z
    temp <- getEmptyLocs x
    print temp
    (getVals x (0,1)) >>= print
    (mapM (\i -> getVals x i) temp) >>= print
    firstFill x >>= arrFromIOArray >>= printArr
    unsafeRead x 0 >>= (\x -> return (x == 3)) >>= print
    solve x >>= arrFromIOArray >>= printArr
    solve x >>= fullCheck >>= print
