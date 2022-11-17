module Problem46 (and', or', nand', nor', xor', equ', table) where


not' :: Bool -> Bool
not' True = False
not' False = True

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' True False = False
equ' False True = False
equ' False False = False

and' :: Bool -> Bool -> Bool
and' x y = x && y

or' :: Bool -> Bool -> Bool
or' x y = x || y

nand' :: Bool -> Bool -> Bool
nand' x y = not' (and' x y)

nor' :: Bool -> Bool -> Bool
nor' x y = not (x || y)

xor' :: Bool -> Bool -> Bool
xor' x y = equ' (not' x) y

show_bool :: Bool -> String
show_bool x = if x then "true" else "fail"

table :: (Bool -> Bool -> Bool) -> IO ()
table f = do
    putStr "true true "
    putStr $ show_bool $ f True True
    putStr "\n"
    putStr "true fail "
    putStr $ show_bool $ f True False
    putStr "\n"
    putStr "fail true "
    putStr $ show_bool $ f False True
    putStr "\n"
    putStr "fail fail "
    putStr $ show_bool $ f False False
    putStr "\n"
