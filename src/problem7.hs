module Problem7 (flatten, NestedList(Elem, List)) where

data NestedList a = Elem a | List [NestedList a]


flatten :: NestedList a -> [a]
flatten (Elem v) = [v]
flatten (List l) = foldl (++) [] (map flatten l)
