module Lab6

where
import Data.List
import System.Random
--import Criterion.Main
import Week6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

{-
	Ex.1 Fast Modular Exponentiation
	See function 'exM' in Week6.hs
-}
	  
{-
	Ex.2 Testing Fast Modular Exponentiation
	
	Using Criterion for benchmarking (cannot run criterion on windows 8, due to mingw 32 linking issues)


main = defaultMain [
		bgroup "exp" [	bench "expM 7 2048 13" $ whnf (expM 7 2048) 13,
						bench "exM 7 2048 13" $ whnf (exM 7 2048) 13
					  ]
		]
	-}	

{- 
	Ex.3 composite number generation
-}

composites = filter (\x -> not $ isPrime x) [4..]
--composites' = filter (\c -> last (takeWhile (\m -> m < c+1) primes) /= c) [4..] --> very slow

{-
	Ex.4 testing Fermat
	when we increase k, the number of which a false positive is given is usually larger, but not always. There is always a chance to find the least composite: 4
-}

ifM p a b = do { p' <- p; if p' then return a else return b }

testF :: Int -> IO Integer
testF k = findPrimeF k composites

--return first encountered composite which is a prime according to primeF
findPrimeF :: Int -> [Integer] -> IO Integer
findPrimeF k (c:cs) = do 
					isP <- primeF k c
					if isP
					then return c
					else findPrimeF k cs
					
testF' :: Int -> [IO Integer]
testF' k = findPrimesF k 100 composites

findPrimesF :: Int -> Integer -> [Integer] -> [IO Integer]
findPrimesF k n (c:cs) = if n == 0
					     then []
					     else do
							 --isP <- primeF k c
							 if True --isP
							 then (return c) : findPrimesF k (n-1) cs
							 else findPrimesF k n cs