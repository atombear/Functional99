module Problem50 (huffman) where

import Data.Sequence (Seq, fromList, insertAt, update, index, lookup, insertAt, deleteAt)
import Data.List (sort)


-- a huffman tree
data Tree a = Node a (Tree a) (Tree a) | Nil deriving (Show, Eq, Ord)


-- comes from processing a heap
type Heap a = Seq a


-- swap two elements, no error handling
swap :: Heap a -> Int -> Int -> Heap a
swap heap idx jdx = (update idx val_jdx) . (update jdx val_idx) $ heap
    where val_idx = index heap idx
          val_jdx = index heap jdx


-- the tree is laid out in a list
getParentNode :: Int -> Int
getParentNode node = div (node - 1) 2


-- sift a value up the tree
siftUpHelper :: Ord a => Heap a -> Int -> Heap a
siftUpHelper heap node
    | (node <= 0 || (index heap parent <= index heap node)) = heap
    | otherwise = siftUpHelper new_heap new_node
    where parent = getParentNode node
          new_heap = swap heap parent node
          new_node = parent

siftUp :: Ord a => Heap a -> Heap a
siftUp heap = siftUpHelper heap node
    where node = (length heap) - 1


-- sift a value at a given index down the tree
siftDown :: Ord a => Heap a -> Int -> Heap a
siftDown heap parent
    | ((c1 >= heap_len || val_parent <= val_c1) &&
       (c2 >= heap_len || val_parent <= val_c2)) = heap

    | otherwise = if c1 < heap_len && c2 < heap_len then
                      if val_c1 < val_c2 then
                          follow_c1
                      else
                          follow_c2
                  else if c1 < heap_len then
                      follow_c1
                  else
                      follow_c2

    where swap_c1 = swap heap c1 parent
          swap_c2 = swap heap c2 parent
          c1 = 2 * parent + 1
          c2 = 2 * parent + 2
          val_parent = index heap parent
          val_c1 = index heap c1
          val_c2 = index heap c2
          heap_len = length heap
          follow_c1 = siftDown swap_c1 c1
          follow_c2 = siftDown swap_c2 c2


-- remove the root
heapRemove :: Ord a => Heap a -> (Heap a, a)
heapRemove heap = (new_heap, val)
    where last_idx = (length heap) - 1
          val = index heap 0
          new_heap = siftDown (deleteAt last_idx (swap heap 0 last_idx)) 0


-- insert an element
heapInsert :: Ord a => Heap a -> a -> Heap a
heapInsert heap val = siftUp (insertAt (length heap) val heap)


-- make a heap by sifting every element, moving backwards through the list
makeHeap :: Ord a => [a] -> Heap a
makeHeap list = foldl (\x -> \y -> siftDown x y) (fromList list) (reverse [0..((length list) - 1)])


-- is heap?
isHeap :: Ord a => Heap a -> Bool
isHeap heap = all id $ map (checkChildren heap) [0..((length heap) - 1)]
    where checkChildren heap node = check_c1 && check_c2
              where node_val = index heap node
                    c1 = 2*node + 1
                    c2 = 2*node + 2
                    c1_maybe = Data.Sequence.lookup c1 heap
                    c2_maybe = Data.Sequence.lookup c2 heap
                    check_c1 = check node_val c1_maybe
                    check_c2 = check node_val c2_maybe
                    check _ Nothing = True
                    check nv (Just c_val) = (nv < c_val)


-- make a leaf
leaf :: a -> Tree a
leaf val = Node val Nil Nil


-- how big is the tree
sizeTree :: Tree a -> Int
sizeTree Nil = 0
sizeTree (Node val left right) = 1 + (sizeTree left) + (sizeTree right)


-- aggregate a heap of leaves into a huffman tree, where each leaf holds a tuple whose elements are
-- 1) an internal node with '_' as a placeholder and the sum of all the weights below
-- 2) a leaf with the character and its appearance in the text
processLanguage :: Heap (Tree (Int, Char)) -> Tree (Int, Char)
processLanguage heap
    | length heap == 1 = snd $ heapRemove heap
    | otherwise = processLanguage new_heap
    where (heap0, el0) = heapRemove heap
          (heap1, el1) = heapRemove heap0
          new_node = Node ((getVal el0 + getVal el1), '_') el0 el1
          new_heap = heapInsert heap1 new_node
          getVal (Node (v, _) _ _) = v


-- construct the coding
processHuffmanTree :: Tree (Int, Char) -> String -> [(Char, String)]
processHuffmanTree huffman_tree code
    | huffman_tree == Nil = []
    | otherwise = if char == '_' then
                      sort $ left_right
                  else
                      sort $ [(char, reverse code)] ++ left_right
    where (Node (val, char) left right) = huffman_tree
          left_right = (processHuffmanTree left ('0':code)) ++ (processHuffmanTree right ('1':code))


-- a wrapper function to manage the types as expressed in the problem statement.
huffman :: [(Char, Int)] -> [(Char, String)]
huffman dictionary = processHuffmanTree huffman_tree ""
    where huffman_tree = processLanguage $ makeHeap $ map leaf $ map (\(x, y) -> (y, x)) dictionary


main = do
    let x = leaf 3
    print x
    let y = Node 5 x Nil
    print $ sizeTree x
    print $ sizeTree y
    print $ (siftUp $ fromList [2,1,7,3])
    let my_heap = makeHeap [35, 39, 43, 19, 56, 77, 18, 4, 77, 90]
    print my_heap
    print $ isHeap my_heap
    print $ heapRemove my_heap
    let my_heap_lang = makeHeap $ map leaf [(45,'a'),(13,'b'),(12,'c'),(16,'d'),(9,'e'),(5,'f')]
    print $ processLanguage my_heap_lang
    print $ huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]