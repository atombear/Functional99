module Problem73 (toLispy, fromLispy) where

import Problem70 (MTree (MNode, MNil))


insert_spaces :: [String] -> String
insert_spaces [] = ""
insert_spaces (x:xs)
    | length xs > 0 = x ++ " " ++ (insert_spaces xs)
    | length xs == 0 = x


toLispy :: MTree Char -> String
toLispy (MNode val node_list)
    | node_list == [] = (val:"")
    | otherwise = "(" ++ (val:" ") ++ (insert_spaces $ map toLispy node_list) ++ ")"


mySplitAt :: Char -> String -> [String]
mySplitAt _ "" = []
mySplitAt sp word = [match] ++ (mySplitAt sp rest)
    where match = takeWhile (\x -> x /= sp) word
          rest = [c | (idx, c) <- zip [0..] word, idx > length match]


mySpaceReplace :: String -> String
mySpaceReplace word = (reverse.fst) repl
    where repl = foldl (\(accum, parens) -> \c -> if length accum > 0 && parens == 0 && c == ' ' then
                                                      (',':accum, parens)
                                                  else if c == '(' then
                                                      (c:accum, parens + 1)
                                                  else if c == ')' then
                                                      (c:accum, parens - 1)
                                                  else
                                                      (c:accum, parens))
                 ("", 0)
                 word


fromLispy :: String -> MTree Char
fromLispy "" = MNil
fromLispy word
    | length word == 1 = MNode (head word) []
    | otherwise = MNode val $ map fromLispy children
    where word_rm_p = (tail.init) word
          val = head word_rm_p
          children = mySplitAt ',' $ mySpaceReplace $ (tail.tail) word_rm_p
