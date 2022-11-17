module Problem70 (MTree (MNode, MNil), num_mnodes, mtreeToString, stringToMTree) where


data MTree a = MNode a [MTree a] | MNil deriving (Show, Eq)


num_mnodes :: MTree a -> Int
num_mnodes MNil = 0
num_mnodes (MNode _ node_list) = 1 + (sum (map num_mnodes node_list))


height :: MTree a -> Int
height MNil = 0
height (MNode _ node_list) = 1 + (maximum (0:map height node_list))


mtreeToString' :: MTree Char -> String
mtreeToString' MNil = ""
mtreeToString' (MNode val node_list) = (val:foldl (++) [] (map tree_and_height node_list))
    where tree_and_height t = (mtreeToString' t) ++ "^"


mtreeToString :: MTree Char -> String
mtreeToString MNil = ""
mtreeToString tree = (mtreeToString' tree) ++ "^"


mySplitAt :: Char -> String -> [String]
mySplitAt _ "" = []
mySplitAt sp word = [match] ++ (mySplitAt sp rest)
    where match = takeWhile (\x -> x /= sp) word
          rest = [c | (idx, c) <- zip [0..] word, idx > length match]


stringToMTree :: String -> MTree Char
stringToMTree "" = MNil
stringToMTree tree_string = MNode val $ map stringToMTree str_list
    where val = head tree_string
          node_strs = (reverse.fst) $ foldl (\(accum, count) -> \c -> if count == 0 then
                                                                          ((c:',':accum),count+1)
                                                                      else (c:accum, if c == '^' then
                                                                                         count - 1
                                                                                     else count + 1))
                      ("",0)
                      ((tail.init) tree_string)
          str_list = if length node_strs > 0 then mySplitAt ',' $ tail node_strs else []
