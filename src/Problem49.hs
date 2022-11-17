module Problem49 (gray, binToInt) where

-- gray :: Int -> [String]
-- gray 0 = []
-- gray 1 = ["0", "1"]
-- gray n = (map (\x -> ('0':x)) last_gray) ++ (map (\x -> ('1':x)) $ reverse last_gray)
--     where last_gray = gray (n-1)


fix :: (a -> a) -> a
fix f = f $ g
    where g = fix f


memo :: (Int -> a) -> (Int -> a)
memo f = (map f [0..] !!)


fixFac :: (Int -> Int) -> (Int -> Int)
fixFac _ 0 = 1
fixFac f n = n * (f (n-1))

fac :: Int -> Int
fac = fix fixFac


fixFib :: (Int -> Int) -> (Int -> Int)
fixFib _ 0 = 1
fixFib _ 1 = 1
fixFib f n = (f (n-1)) + (f (n-2))

fib :: Int -> Int
fib = fix (memo . fixFib)


fixGray :: (Int -> [String]) -> (Int -> [String])
fixGray _ 0 = []
fixGray _ 1 = ["0", "1"]
fixGray f n = (map (\x -> ('0':x)) last_gray) ++ (map (\x -> ('1':x)) $ reverse last_gray)
    where last_gray = fixGray f (n-1)

gray :: Int -> [String]
gray = fix (memo . fixGray)


binToInt :: String -> Int
binToInt word = sum $ map (\(x, y) -> (read (x:"") * 2^y)) (zip word [0..])
