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

testF :: Int -> IO Integer
testF k = findPrimeF k composites

--return first encountered composite which is a prime according to primeF
findPrimeF :: Int -> [Integer] -> IO Integer
findPrimeF k (c:cs) = do 
					isPrime <- primeF k c
					if isPrime 
					then return c
					else testF' k c