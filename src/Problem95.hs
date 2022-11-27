module Problem95 (fullWords) where


writDigits :: [String]
writDigits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]


join :: Char -> [String] -> String
join el words = concat $ (head words):(map (el:) (tail words))


digits :: Int -> [Int]
digits num = reverse $ map (\x -> mod x 10) $ takeWhile (\x -> x > 0) [div num (10^n) | n <- [0..]]


fullWords :: Int -> String
fullWords num = join '-' $ map (\idx -> writDigits !! idx) $ digits num
