import Problem55 (cbalTree, toList, Tree (Node, Nil))
import Problem56 (symmetricTree)
import Problem57 (constructBST)
import Problem58 (symCbalTrees)
import Problem59 (hbalTree, isBalanced)
import Problem60 (hbalTreeNodes)
import Problem61 (countLeaves, collectLeaves)
import Problem62 (internals, atLevel)
import Problem63 (completeBinaryTree, isCompleteBinaryTree)
import Problem64 (layout)
import Problem65 (layoutBin)
import Problem66 (layoutTight)
import Problem67 (stringToTree, treeToString)
import Problem68 (preOrder, inOrder, treeFromPreIn)
import Problem69 (tree2ds, ds2tree)
import Problem70 (MTree (MNode, MNil), num_mnodes, mtreeToString, stringToMTree)
import Problem71 (ipl)
import Problem72 (bottom_up)
import Problem73 (toLispy, fromLispy)
import Problem80 (EdgeGraph, AdjGraph, transEdgeToAdj, transAdjToEdge)
import Problem81 (paths)
import Problem82 (graph_cycle)
import Problem83 (det, isTree, countKirchoff, spanningTrees, processGraph)
import Problem84 (prim)
import Problem85 (iso, isoBrute)
import Problem86 (degree, degreeSort, colorWelshPowell)
import Problem87 (depthFirst)
import Problem88 (connectedComponents)
import Problem89 (bipartite)
import Problem90 (queens)
import Problem91 (findTour)


import qualified Data.Map as Map (Map, fromList, toList)
import qualified Data.Set as Set (Set, fromList, toList)

adjFromList x = (transEdgeToAdj (Set.fromList x))

adj_graph81a = adjFromList [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
adj_graph83a = adjFromList [(1,2), (2,3), (1,4)]
adj_graph83b = adjFromList [(0,1), (1,2), (1,3), (1,4)]
adj_graph83c = adjFromList [(1,2), (2,3), (1, 3)]
adj_graph83d = adjFromList [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]
adj_graph83e = adjFromList [('a','b'),('a','d'),('b','e'),('b','c'),
                            ('c','e'),('e','h'),('e','d'),('h','g'),
                            ('g','d'),('g','f'),('f','d')]
adj_graph85a = adjFromList [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),
                            (3,8),(4,6),(4,7),(4,8)]
adj_graph85b = adjFromList [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),
                            (8,7),(3,2),(3,4),(3,7)]

adj_graph86a = adjFromList [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),('c','d'),('c','h'),('d','e'),
                            ('d','i'),('e','j'),('f','h'),('f','i'),('g','i'),('g','j'),('h','j')]

adj_graph87a = adjFromList [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]

adj_graph88a = (processGraph.adjFromList) [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]

adj_graph89a = adjFromList [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)]
adj_graph89b = adjFromList [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(1,3)]


tree4 :: Tree Int
tree4 = Node 1 (Node 2 Nil (Node 4 Nil Nil))
               (Node 2 Nil Nil)


tree64 = Node 'n'
                (Node 'k'
                        (Node 'c'
                                (Node 'a' Nil Nil)
                                (Node 'h'
                                        (Node 'g'
                                                (Node 'e' Nil Nil)
                                                Nil
                                        )
                                        Nil
                                )
                        )
                        (Node 'm' Nil Nil)
                )
                (Node 'u'
                        (Node 'p'
                                Nil
                                (Node 's'
                                        (Node 'q' Nil Nil)
                                        Nil
                                )
                        )
                        Nil
                )


tree65 = Node 'n'
                (Node 'k'
                        (Node 'c'
                                (Node 'a' Nil Nil)
                                (Node 'e'
                                        (Node 'd' Nil Nil)
                                        (Node 'g' Nil Nil)
                                )
                        )
                        (Node 'm' Nil Nil)
                )
                (Node 'u'
                        (Node 'p'
                                Nil
                                (Node 'q' Nil Nil)
                        )
                        Nil
                )



mtree1 = MNode 'a' []

mtree2 = MNode 'a' [MNode 'b' []]

mtree3 = MNode 'a' [MNode 'b' [MNode 'c' []]]

mtree4 = MNode 'b' [MNode 'd' [], MNode 'e' []]

mtree5 = MNode 'a' [MNode 'f' [MNode 'g' []],
                    MNode 'c' [],
                    MNode 'b' [MNode 'd' [], MNode 'e' []]
                   ]



assert :: Bool -> IO ()
assert True = return ()
assert False = do
    error "broken test!"
    return ()


getUnique :: Eq a => [a] -> [a]
getUnique = foldl (\x -> \y -> if (elem y x) then x else (y:x)) []


isUnique :: Eq a => [a] -> Bool
isUnique = \x -> (length . getUnique) x == length x


isAlphabetical :: Ord a => [a] -> Bool
isAlphabetical (x:xs) = (length xs == 0) || (x <= head xs) && (isAlphabetical xs)


compareSets :: Eq a => [a] -> [a] -> Bool
compareSets s0 s1
    | length s0 /= length s1 = False
    | otherwise = (all (\x -> elem x s0) s1) && (all (\x -> elem x s1) s0)


listPrint :: Show a => [a] -> IO ()
listPrint [] = return ()
listPrint (x:xs) = do
    print x
    listPrint xs


binMaybe :: Maybe a -> Int
binMaybe (Just _) = 1
binMaybe Nothing = 0


main = do
    print "problem55"
    let compl_bal_4 = map (map binMaybe) $ map toList $ cbalTree 4
    assert (compl_bal_4 == [[1,1,1,1,0,0,0], [1,1,1,0,1,0,0], [1,1,1,0,0,1,0], [1,1,1,0,0,0,1]])

    print "problem56"
    assert (symmetricTree (Node 'x' (Node 'x' Nil Nil) Nil) == False)
    assert (symmetricTree (Node 'x' (Node 'x' Nil Nil) (Node 'x' Nil Nil)))

    print "problem57"
    assert $ (constructBST [3, 2, 5, 7, 1] == Node 3 (Node 2 (Node 1 Nil Nil) Nil) (Node 5 Nil (Node 7 Nil Nil)))
    assert $ (symmetricTree . constructBST $ [5, 3, 18, 1, 4, 12, 21])
    assert $ (symmetricTree . constructBST $ [3, 2, 5, 7, 1])

    print "problem58"
    assert $ (symCbalTrees 5 == reverse [Node 'x' (Node 'x' Nil (Node 'x' Nil Nil)) (Node 'x' (Node 'x' Nil Nil) Nil),
                                         Node 'x' (Node 'x' (Node 'x' Nil Nil) Nil) (Node 'x' Nil (Node 'x' Nil Nil))])

    print "problem59"
    assert (all ((==)True) $ map isBalanced $ hbalTree 4)

    print "problem60"
    assert ((length $ hbalTreeNodes 15) == 1553)

    print "problem61"
    assert $ countLeaves tree4 == 2
    assert $ collectLeaves tree4 == [4, 2]

    print "problem62"
    assert $ internals tree4 == [1, 2]
    assert $ (atLevel tree4 2) == [2, 2]

    print "problem63"
    assert $ completeBinaryTree 4 == Node 'x' (Node 'x' (Node 'x' Nil Nil) Nil) (Node 'x' Nil Nil)
    assert $ isCompleteBinaryTree $ Node 'x' (Node 'x' Nil Nil) (Node 'x' Nil Nil)

    print "problem64"
    assert $ (layout tree64) == Node ('n',(8,1)) (Node ('k',(6,2)) (Node ('c',(2,3)) (Node ('a',(1,4)) Nil Nil) (Node ('h',(5,4)) (Node ('g',(4,5)) (Node ('e',(3,6)) Nil Nil) Nil) Nil)) (Node ('m',(7,3)) Nil Nil)) (Node ('u',(12,2)) (Node ('p',(9,3)) Nil (Node ('s',(11,4)) (Node ('q',(10,5)) Nil Nil) Nil)) Nil)

    print "problem65"
    assert $ layoutBin tree65 == Node ('n',(15,1)) (Node ('k',(7,2)) (Node ('c',(3,3)) (Node ('a',(1,4)) Nil Nil) (Node ('e',(5,4)) (Node ('d',(4,5)) Nil Nil) (Node ('g',(6,5)) Nil Nil))) (Node ('m',(11,3)) Nil Nil)) (Node ('u',(23,2)) (Node ('p',(19,3)) Nil (Node ('q',(21,4)) Nil Nil)) Nil)

    print "problem66"
    assert $ layoutTight (Node 1 Nil Nil) == (Node (1,(1,1)) Nil Nil)
    assert $ layoutTight (Node 1 (Node 2 Nil Nil) Nil) == (Node (1,(2,1))
                                                              (Node (2,(1,2)) Nil Nil)
                                                              Nil)
    assert $ layoutTight tree4 == (Node (1,(2,1))
                                      (Node (2,(1,2))
                                          Nil
                                          (Node (4,(2,3)) Nil Nil))
                                      (Node (2,(3,2)) Nil Nil))
    assert $ layoutTight tree65 == (Node ('n',(5,1))
                                       (Node ('k',(3,2))
                                           (Node ('c',(2,3))
                                               (Node ('a',(1,4)) Nil Nil)
                                               (Node ('e',(3,4))
                                                   (Node ('d',(2,5)) Nil Nil)
                                                   (Node ('g',(4,5)) Nil Nil)))
                                           (Node ('m',(4,3)) Nil Nil))
                                       (Node ('u',(7,2))
                                           (Node ('p',(6,3))
                                               Nil
                                               (Node ('q',(7,4)) Nil Nil))
                                           Nil))

    print "problem67"
    assert $ stringToTree "x(y,a(,b))" == Node 'x' (Node 'y' Nil Nil) (Node 'a' Nil (Node 'b' Nil Nil))
    assert $ (treeToString $ Node 'x' (Node 'y' Nil Nil) (Node 'a' Nil (Node 'b' Nil Nil))) == "x(y,a(,b))"

    print "problem68"
    assert $ (preOrder $ stringToTree "a(b(d,e),c(,f(g,)))") == "abdecfg"
    assert $ ((\x -> treeFromPreIn (head x) (last x)) [f $ stringToTree "a(b(d,e),c(,f(g,)))" | f <- [preOrder, inOrder]]) == (Node 'a' (Node 'b' (Node 'd' Nil Nil) (Node 'e' Nil Nil)) (Node 'c' Nil (Node 'f' (Node 'g' Nil Nil) Nil)))

    print "problem69"
    assert $ tree2ds (Node 'x' (Node 'y' Nil Nil) (Node 'z' (Node '0' Nil Nil) Nil)) == "xy..z0..."
    assert $ (tree2ds $ stringToTree "x(y,z(0,))") == "xy..z0..."
    assert $ tree2ds (Node 'x' Nil (Node 'z' (Node '0' Nil Nil) Nil)) == "x.z0..."
    assert $ (tree2ds $ stringToTree "a(b(d,e),c(,f(g,)))") == "abd..e..c.fg..."
    assert $ ds2tree "abd..e..c.fg..." == stringToTree "a(b(d,e),c(,f(g,)))"
    assert $ stringToTree "x(y,z(0,))" == ds2tree "xy..z0..."

    print "problem70"
    assert $ num_mnodes mtree1 == 1
    assert $ num_mnodes mtree2 == 2
    assert $ num_mnodes mtree3 == 3
    assert $ num_mnodes mtree4 == 3
    assert $ num_mnodes mtree5 == 7

    assert $ mtreeToString ((MNode 'a' [MNode 'f' [MNode 'g' []], MNode 'c' [], MNode 'b' [MNode 'd' [], MNode 'e' []]])) == "afg^^c^bd^e^^^"

    assert $ stringToMTree "afg^^c^bd^e^^^" == (MNode 'a' [MNode 'f' [MNode 'g' []], MNode 'c' [], MNode 'b' [MNode 'd' [], MNode 'e' []]])

    print "problem71"
    assert $ ipl mtree5 == 9
    assert $ ipl mtree4 == 2

    print "problem72"
    assert $ bottom_up mtree5 == "gfcdeba"

    print "problem73"
    assert $ toLispy mtree1 == "a"
    assert $ toLispy mtree2 == "(a b)"
    assert $ toLispy mtree3 == "(a (b c))"
    assert $ toLispy mtree4 == "(b d e)"
    assert $ toLispy mtree5 == "(a (f g) c (b d e))"

    assert $ fromLispy "a" == mtree1
    assert $ fromLispy "(a b)" == mtree2
    assert $ fromLispy "(a (b c))" == mtree3
    assert $ fromLispy "(b d e)" == mtree4
    assert $ fromLispy "(a (f g) c (b d e))" == mtree5

    print "problem80"
    assert $ transEdgeToAdj (Set.fromList [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]) == (Map.fromList [('b', Set.fromList "cf"),('c', Set.fromList "f"),('f', Set.fromList "k"),('g', Set.fromList "h"),('h', Set.fromList ""),('k', Set.fromList "")])

    assert $ transAdjToEdge (Map.fromList [('b', Set.fromList "cf"),('c', Set.fromList "bf"),('f', Set.fromList "bck"),('g', Set.fromList "h"),('h', Set.fromList "g"),('k', Set.fromList "f")]) == Set.fromList [('b','c'),('b','f'),('c','b'),('c','f'),('f','b'),('f','c'),('f','k'),('g','h'),('h','g'),('k','f')]

    print "problem81"
    assert $ paths 1 4 adj_graph81a == [[1,2,3,4],[1,3,4]]
    assert $ paths 2 6 adj_graph81a == []

    print "problem82"
    assert $ graph_cycle 2 adj_graph81a == [[2,3,4,2]]
    assert $ graph_cycle 1 adj_graph81a == []

    print "problem83"
    assert $ isTree adj_graph83a
    assert $ isTree adj_graph83b
    assert $ not $ isTree adj_graph83c

    assert $ countKirchoff adj_graph83a == 1
    assert $ countKirchoff adj_graph83d == 16
    assert $ (length $ spanningTrees adj_graph83a) == 1
    assert $ (length $ spanningTrees adj_graph83d) == 16

    assert $ countKirchoff adj_graph83e == 112
    assert $ (length $ spanningTrees adj_graph83e) == 112

    print "problem84"

    let graph_list_83e = [('a','b'),('a','d'),('b','e'),('b','c'),
                          ('c','e'),('e','h'),('e','d'),('h','g'),
                          ('g','d'),('g','f'),('f','d')]
    let edge_list_83e = [5,3,4,2,6,5,7,1,3,4,4]

    assert $ ((prim $ zip graph_list_83e edge_list_83e)
                    == (Map.fromList [('a',Set.fromList "d"),('b',Set.fromList "ce"),
                                      ('c',Set.fromList "b"),('d',Set.fromList "afg"),
                                      ('e',Set.fromList "bh"),('f',Set.fromList "d"),
                                      ('g',Set.fromList "dh"),('h',Set.fromList "eg")]))

    print "problem85"
    assert $ iso adj_graph85a adj_graph85b
    assert $ not $ iso adj_graph85a adj_graph83a

    print "problem86"
    assert $ colorWelshPowell adj_graph86a == [('a',0),('c',0),('i',0),('j',0),('e',1),('g',1),('h',1),('b',2),('d',2),('f',2)]

    print "problem87"
    assert $ depthFirst adj_graph87a 1 == [1,2,3,4]

    print "problem88"
    assert $ connectedComponents adj_graph88a == [[6,7],[1,2,3,4,5]]

    print "problem89"
    assert $ bipartite adj_graph89a
    assert $ (not.bipartite) adj_graph89b

    print "problem90"
    assert $ (length.queens) 8 == 92

    print "problem91"
    assert $ findTour 8 == [[0,37,58,35,42,47,56,51],[59,34,1,48,57,50,43,46],[38,31,36,41,2,45,52,55],[33,60,39,26,49,54,3,44],[30,9,32,61,40,25,22,53],[17,62,27,10,23,20,13,4],[8,29,18,15,6,11,24,21],[63,16,7,28,19,14,5,12]]

    print "fin"
