module Problem36 (prime_factors_mult) where

import Data.Map (Map, fromList, toList)
import Problem35 (primeFactors)
import Problem28 (adjustWithDefault)


prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult num = toList $ foldl (\x -> \y -> adjustWithDefault (+1) y 1 x) (fromList []) (primeFactors num)
