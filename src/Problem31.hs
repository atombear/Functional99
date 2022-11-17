module Problem31 (isPrime) where


isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime val = all (\x -> mod val x /= 0) [2..(val-1)]