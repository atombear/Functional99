module Problem67 (stringToTree, treeToString) where

import Problem55 (Tree (Node, Nil))


splitFirst :: String -> Char -> (String, String)
splitFirst word delim = (first, second)
    where first = [c | (idx, c) <- zip [0..] word, idx < loc]
          second = [c | (idx, c) <- zip [0..] word, idx > loc]
          (_, loc) = foldl (\(num_paren, loc) -> \(c, idx) -> if c == ')' then
                                                                  (num_paren-1, loc)
                                                              else if c == '(' then
                                                                  (num_paren+1, loc)
                                                              else if (num_paren == 0 && c ==',') then
                                                                  (num_paren, idx)
                                                              else
                                                                  (num_paren, loc))
                           (0, -1)
                           (zip word [0..])


stringToTree :: String -> Tree Char
stringToTree "" = Nil
stringToTree word = Node val left right
    where val = head word
          rest = if elem '(' word then (tail.tail.init) word else ""
          (left_string, right_string) = splitFirst rest ','
          left = stringToTree left_string
          right = stringToTree right_string


treeToString :: Tree Char -> String
treeToString Nil = ""
treeToString (Node val left right) = (val:"") ++ children
    where possible_children = "(" ++ (treeToString left) ++ "," ++ (treeToString right) ++ ")"
          children = if possible_children == "(,)" then "" else possible_children
