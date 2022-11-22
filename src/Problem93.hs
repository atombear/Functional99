module Problem93 (arith, processExpr, processLevel, checkExpr) where

import Debug.Trace (trace)


div' :: Integral a => a -> a -> a
div' n0 n1 = case (n0 < 0, n1 < 0) of
                 (False, False) -> div n0 n1
                 (True, True) -> div (abs n0) (abs n1)
                 _ -> (-1) * (div (abs n0) (abs n1))


splitAt' :: [a] -> Int -> ([a], [a])
splitAt' list num = (take num list, reverse $ take (length list - num) $ reverse list)


splitAtEl :: Eq a => [a] -> a -> ([a], [a])
splitAtEl list el = (lhs, rhs)
    where lhs = takeWhile (\x -> x /= el) list
          rhs = reverse $ take ((length list) - (length lhs) - 1) $ reverse list


produceExpr :: String -> String -> String -> String
produceExpr lnum rnum opstr = "(" ++ lnum ++ opstr ++ rnum ++ ")"


produceExprList :: Int -> String -> Int -> String -> [(Int, String)]
produceExprList v0 s0 v1 s1
    | v1 == 0 = [(v0+v1, produceExprN0N1 "+"),
                 (v0-v1, produceExprN0N1 "-"),
                 (v0*v1, produceExprN0N1 "*")]
    | otherwise = [(v0+v1, produceExprN0N1 "+"),
                   (v0-v1, produceExprN0N1 "-"),
                   (div' v0 v1, produceExprN0N1 "/"),
                   (v0*v1, produceExprN0N1 "*")]
    where produceExprN0N1 = produceExpr s0 s1


combine :: (Int, String) -> (Int, String) -> [(Int, String)]
combine (v0, s0) (v1, s1) = produceExprList v0 s0 v1 s1



arith' :: [Int] -> [(Int, String)]
arith' [] =[]
arith' [v] = [(v, show v)]
arith' [v0, v1] = produceExprList v0 (show v0) v1 (show v1)
arith' vs = (concat.concat) splits
    where splits = map (\(i, j) -> combProd (arith' i) (arith' j)) $ map (splitAt' vs) [1..(length vs) - 1]
          combProd exprs0 exprs1 = [combine e0 e1 | e0 <- exprs0, e1 <- exprs1]


arith :: [Int] -> [String]
arith [] = []
arith [_] = []
arith [v0, v1] = if v0 == v1 then [(show v0) ++ "=" ++ (show v1)] else []
arith vs = concat $ map (\(i, j) -> [(snd lhs) ++ "=" ++ (snd rhs) | lhs <- i, rhs <- j, (fst lhs) == (fst rhs)]) splits
    where splits = map (\(i, j) -> (arith' i, arith' j)) $ map (splitAt' vs) [1..(length vs)-1]


mapOp :: Integral a => Char -> (a -> a -> a)
mapOp '+' = (+)
mapOp '-' = (-)
mapOp '*' = (*)
mapOp '/' = div'


getNumStr :: String -> String
getNumStr expr = if head expr == '-' then
                     ('-':(takeWhile (\x -> not (elem x "+-/*")) (tail expr)))
                 else
                     takeWhile (\x -> not (elem x "+-/*")) expr


parseExpr :: String -> (String, Char, String)
parseExpr expr = (first_num_str, op, second_num_str)
    where first_num_str = getNumStr expr
          (_, (op:rest)) = splitAt' expr (length first_num_str)
          second_num_str = getNumStr rest

evalRaw :: String -> Int
evalRaw expr
    | (not.(any id)) (map (\x -> elem x (if head expr == '-' then tail expr else expr)) "+-/*") = (read expr :: Int)
    | otherwise = evalRaw (eval ++ rest)
    where (fns, op, sns) = parseExpr expr
          rest = reverse $ take (length expr - (length fns + 1 + length sns)) $ reverse expr
          eval = show ((mapOp op) (read fns :: Int) (read sns :: Int))


exprToList :: String -> [String]
exprToList expr = filter (\x -> length x > 0) $ reverse $ map reverse $ parse
    where parse = foldl (\accum -> \el -> if elem el "+-*/" then ("":(el:""):accum) else ((el:(head accum)):(tail accum))) [""] expr


stringApply :: Char -> String -> String -> String
stringApply op nstr0 nstr1 = show ((mapOp op) (read nstr0 :: Int) (read nstr1 :: Int))

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

evalMD :: [String] -> [String]
evalMD numops = (reverse.snd3) $ foldl (\(fMD, accum, last_el) -> \el ->
                                           if (el == "*" || el == "/") then
                                               (stringApply (head el) (head accum), tail accum, el)
                                           else if el == "-" && (last_el == "*" || last_el == "/")then
                                               (\x -> stringApply '*' "-1" (fMD x), accum, el)
                                           else
                                               (id, (fMD el):accum, el))
                                       (id, [], "")
                                       numops

processMD :: String -> String
processMD = join.evalMD.exprToList
    where join = foldl (++) ""


processExpr :: String -> Int
processExpr = evalRaw.processMD


cntP :: Char -> Int
cntP el = case el of
              ')' -> -1
              '(' -> 1
              _   -> 0


minLevel :: String -> Int
minLevel expr = snd $ foldl (\(cnt, min_cnt) -> \el ->
                                let new_cnt = cnt + (cntP el)
                                in (new_cnt, min new_cnt min_cnt))
                            (0, length expr)
                            (init expr)


maxLevel :: String -> Int
maxLevel expr = snd $ foldl (\(cnt, max_cnt) -> \el ->
                                let new_cnt = cnt + (cntP el)
                                in (new_cnt, max new_cnt max_cnt))
                            (0, 0)
                            expr


rmParens :: String -> String
rmParens expr
    | minLevel expr == 0 = expr
    | otherwise = (tail.init) expr


fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

processLevel :: String -> String
processLevel expr
    | not (elem '(' expr) = (show.processExpr) expr
    | otherwise = (processLevel.fst3) $
                      foldl (\(accum, lvlcache, cnt) -> \el ->
                                let new_cnt = cnt + (cntP el)
                                in
                                    if new_cnt == max_level then
                                        (accum, el:lvlcache, new_cnt)
                                    else if length lvlcache > 0 then
                                        (accum ++ ((show.processExpr.tail.reverse) lvlcache), "", new_cnt)
                                    else
                                        (accum ++ (el:""), "", new_cnt))
                      ("", "", 0)
                      expr
    where max_level = maxLevel expr

 
checkExpr :: String -> Bool
checkExpr expr = (processLevel lhs) == (processLevel rhs)
    where (lhs, rhs) = splitAtEl expr '='
