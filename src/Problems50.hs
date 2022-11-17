import Problem1 (mylast)
import Problem2 (myButLast)
import Problem3 (elementAt)
import Problem4 (myLength)
import Problem5 (myReverse)
import Problem6 (isPalindrome)
import Problem7 (flatten, NestedList(Elem, List))
import Problem8 (compress)
import Problem9 (pack)
import Problem10 (encode)
import Problem11 (encode_modified, Degeneracy(Multiple, Single))
import Problem12 (decode_modified)
import Problem13 (encodeDirect)
import Problem14 (dupli)
import Problem15 (repli)
import Problem16 (dropEvery)
import Problem17 (split)
import Problem18 (slice)
import Problem19 (rotate)
import Problem20 (removeAt)
import Problem21 (insertAt)
import Problem22 (range)
import Problem23 (rnd_select)
import Problem24 (diff_select)
import Problem25 (rnd_permu)
import Problem26 (combinations, choose)
import Problem27 (group)
import Problem28 (lsort, lfsort, qsort)
import Problem31 (isPrime)
import Problem32 (myGCD)
import Problem33 (coprime)
import Problem34 (totient)
import Problem35 (primeFactors)
import Problem36 (prime_factors_mult)
import Problem37 (phi)
import Problem39 (primesR)
import Problem40 (goldbach)
import Problem41 (goldbachList, goldbachList')
import Problem46 (and', or', equ', table)
import Problem48 (tablen)
import Problem49 (gray, binToInt)
import Problem50 (huffman)


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


main = do
    print "problem1"
    assert $ mylast [1,2,3,4] == 4
    assert $ mylast ['x','y','z'] == 'z'

    print "problem2"
    assert $ myButLast [1,2,3,4] == 3
    assert $ myButLast ['a'..'z'] == 'y'

    print "problem3"
    assert $ elementAt [1,2,3] 2 == 2
    assert $ elementAt "haskell" 5 == 'e'
    assert $ elementAt "123" 3 == '3'

    print "problem4"
    assert $ myLength [] == 0
    assert $ myLength [89, 98, 19] == 3
    assert $ myLength ([11] ++ [13, 17]) == 3

    print "problem5"
    assert $ myReverse "A man, a plan, a canal, panama!" == "!amanap ,lanac a ,nalp a ,nam A"
    assert $ myReverse [1,2,3,4] == [4,3,2,1]
    assert $ (myReverse [] :: [Int]) == ([] :: [Int])

    print "problem6"
    assert $ not (isPalindrome [1,2,3])
    assert $ isPalindrome "madamimadam"
    assert $ isPalindrome [1,2,4,8,16,8,4,2,1]

    print "problem7"
    assert $ flatten (Elem 5) == [5]
    assert $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) == [1,2,3,4,5]
    assert $ (length $ flatten (List [])) == 0

    print "problem8"
    assert $ compress "aaaabccaadeeee" == "abcade"

    print "problem9"
    assert $ pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] == ["aaaa","b","cc","aa","d","eeee"]

    print "problem10"
    assert $ encode "aaaabccaadeeee" == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
    assert $ (length $ encode "") == 0

    print "problem11"
    assert $ encode_modified "aaaabccaadeeee" == [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

    print "problem12"
    assert $ decode_modified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e'] == "aaaabccaadeeee"
    
    print "problem13"
    assert $ encodeDirect "aaaabccaadeeee" == [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

    print "problem14"
    assert $ dupli [1, 2, 3] == [1,1,2,2,3,3]

    print "problem15"
    assert $ repli "abc" 3 == "aaabbbccc"

    print "problem16"
    assert $ dropEvery "abcdefghik" 3 == "abdeghk"

    print "problem17"
    assert $ split "abcdefghik" 3 == ("abc", "defghik")
    assert $ split "ABCD" 0 == ("", "ABCD")
    assert $ split "ABCD" 4 == ("ABCD", "")

    print "problem18"
    assert $ slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 == "cdefg"

    print "problem19"
    assert $ rotate ['a','b','c','d','e','f','g','h'] 3 == "defghabc"
    assert $ rotate ['a','b','c','d','e','f','g','h'] (-2) == "ghabcdef"


    print "problem20"
    assert $ removeAt 2 "abcd" == ('b',"acd")

    print "problem21"
    assert $ insertAt 'X' "abcd" 2 == "aXbcd"

    print "problem22"
    assert $ range 4 9 == [4..9]
    assert $ range 5 5 == [5]

    print "problem23"
    rand_letters <- sequence [(rnd_select "ABCDEFGH" 5) | _ <- [1..100]]
    let all_rand_letters = foldl (++) [] rand_letters
        unique_letters = getUnique all_rand_letters
    assert $ all isUnique rand_letters    
    assert $ all (\x -> elem x unique_letters) "ABCDEFGH"
    assert $ length unique_letters == length "ABCDEFGH"

    print "problem24"
    random_numbers <- sequence [(diff_select 8 23) | _ <- [1..100]]
    assert $ all (\x -> length x == 8) random_numbers
    let all_random_numbers = foldl (++) [] random_numbers
        unique_numbers = foldl (\x -> \y -> if (elem y x) then x else (y:x)) [] all_random_numbers
    assert $ all (\x -> elem x unique_numbers) [1..23]
    assert $ length unique_numbers == 23

    print "problem25"
    random_permutations <- sequence [rnd_permu "abcdef" | _ <- [1..10000]]
    let unique_permutations = getUnique random_permutations
    assert $ length unique_permutations == 720

    print "problem26"
    let combs_list = combinations 3 "abcdefgh"
    assert $ length combs_list == choose 8 3    
    assert $ all isAlphabetical combs_list
    assert $ (length $ getUnique combs_list) == length combs_list

    print "problem27"
    let vals0 = ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
    let groups0 = group [2,3,4] vals0
    assert $ all isAlphabetical $ foldl (++) [] groups0
    assert $ length groups0 == (choose 9 2) * (choose 7 3)
    assert $ all (\x -> compareSets (foldl (++) [] x) vals0) groups0

    print "problem28"
    let sorted0 = lsort ["abc","de","fgh","de","ijkl","mn","o"]
    assert $ isAlphabetical $ map length sorted0

    let sorted1 = lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
    assert $ map length sorted1 == [4, 1, 3, 3, 2, 2, 2] || map length sorted1 == [1, 4, 3, 3, 2, 2, 2]

    print "problem31"
    assert $ filter isPrime [2..100] == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

    print "problem32"
    assert $ [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6] == [9, 3, 3]

    print "problem33"
    assert $ coprime 35 64

    print "problem34"
    assert $ totient 10 == 4

    print "problem35"
    assert $ primeFactors 315 == [3,3,5,7]
    assert $ primeFactors 2 == [2]
    assert $ primeFactors 4 == [2,2]
    assert $ primeFactors 8 == [2,2,2]
    assert $ primeFactors 98 == [2, 7, 7]

    print "problem36"
    assert $ prime_factors_mult 315 == [(3,2),(5,1),(7,1)]

    print "problem37"
    assert $ phi 10 == 4
    assert $ phi 10090 == totient 10090


    print "problem39"
    assert $ primesR 10 20 == [11,13,17,19]

    print "problem40"
    assert $ goldbach 28 == (5, 23)
    assert $ goldbach 133 == (2,131)
    assert $ goldbach 12348  == (5,12343)

    print "problem41"
    assert $ goldbachList 9 20 == [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
    assert $ goldbachList' 4 2000 50 == [(73,919),(61,1321),(67,1789),(61,1867)]
    assert $ goldbachList' 2 3000 50 == [(73,919),(61,1321),(67,1789),(61,1867),(61,2017),(61,2377),(53,2459),(53,2477),(61,2557),(103,2539)]

    print "problem46"
    table (\a b -> and' a (or' a b))

    print "problem47"
    table (\a b -> a `and'` (a `or'` b))


    print "problem48"
    tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)

    print "problem49"
    assert $ gray 3 == ["000","001","011","010","110","111","101","100"]
    assert $ (qsort (map binToInt $ gray 15) (<)) == [0..(2^15)-1]

    print "problem50"
    assert $ huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)] == [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
    
    print $ "fin"
