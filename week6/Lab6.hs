module Lab6

where
import Data.List
import System.Random
--import Test.QuickCheck
--import Test.QuickCheck.Monadic
--import Criterion.Main
import Week6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

{-	===============================
	Ex.1 Fast Modular Exponentiation
	See function 'exM' in Week6.hs
-}
	  
{-	========================================
	Ex.2 Testing Fast Modular Exponentiation
	
	Using Criterion for efficiency benchmarking (cannot run criterion on windows 8, due to mingw 32 linking issues)

	main = defaultMain [
		bgroup "exp" [	bench "expM 7 2048 13" $ whnf (expM 7 2048) 13,
						bench "exM 7 2048 13" $ whnf (exM 7 2048) 13
					  ]
		]

	Result: does not give benefit in terms of program efficiency, but larger input range:

	> expM 2 1000000 5
	>> segfault/bus error (or crash otherwise)

	> exM  2 1000000 5
	>> 1 

-}	

{-  ================================
	Ex.3 composite number generation
-}

composites = filter (\x -> not $ isPrime x) [4..]


{-	===========
	Ex.4 Fermat
	
	Increasing k results in a larger chance of finding a correct positive in primeF. So, when we increase k, the size of the 'prime' we find is usually larger than with a lower k.
	There is always a chance to find the least composite: 4
-}

-- return first encountered composite which is a prime according to primeF
testF :: Int -> IO Integer
testF k = findPrimeF k composites

findPrimeF :: Int -> [Integer] -> IO Integer
findPrimeF k (c:cs) = do 
					  isP <- primeF k c
					  if isP
					  then return c
					  else findPrimeF k cs

					  
-- test first 10000 lookups in the composite list on primeF method and 
-- print those who primeF indicates to be a prime
testComps :: Int -> IO()
testComps k = printList $ getListF 0 10000 k composites []

printList :: IO [Integer] -> IO()
printList xs = xs >>= print
									
getListF :: Int -> Int -> Int -> [Integer] -> [Integer] -> IO [Integer]
getListF n max k xs ps = if n == max
					     then return $ reverse ps
					     else do 
							  isP <- primeF k (xs!!n)
							  if isP
							  then getListF (n+1) max k xs ((xs!!n):ps)
							  else getListF (n+1) max k xs ps			

{- increasing k, results in a lesser chance of finding false positives, for example:  
	
testComps 1
[8,9,21,25,30,39,117,153,169,231,385,481,527,671,1032,1105,1247,1261,1285,1431,1551,1661,1729,1852,1891,2047,2163,2185,2242,2465,2641,2701,2871,2945,3133,3283,3367,4034,4681,4991,5461,6273,6409,6533,6541,6545,6601,7107,7161,7613,7831,8149,8295,8989,9331,9361,9453,9471,10143,10171,10523,10585,10681,11041]

testComps 2
[85,403,703,1105,2465,2821,4033,5551,6601,7363,8695,10585]

testComps 3
[6601,8911,10585]
-}

							  
-- test 10000 random lookups in the composite list on primeF method and 
-- print those who primeF indicates to be a prime
testRndComps :: Int -> Int -> IO()
testRndComps max k = printList $ getRndListF 10000 max k composites []
							  
getRndListF :: Int -> Int -> Int -> [Integer] -> [Integer] -> IO [Integer]
getRndListF 0 max k xs ps = return $ reverse ps
getRndListF n max k xs ps =  do 
							 idx <- randomRIO (0, max)
							 isP <- primeF k (xs!!idx)
							 if isP
							 then getRndListF (n-1) max k xs ((xs!!idx):ps)
							 else getRndListF (n-1) max k xs ps

{-
	The same conclusion holds for randomly picked composites:
	
	*Lab6> testRndComps 10000 1 	[10011,3245,2465,6697,7345,153,1267,1876,6281,1027,248,2896,1921,5713,4123,3605,10735,10585,2465,21,38,152,7449,165,8911,7345,1027,65,361,9191,6925,11305,2465,93,49,6532,11145,671,6601,8911,7865,12,4233,4636,4961,903,9073,1105,95,3277,831,5617,9889,590,65,3059,10309,1729,561,3309,87,483,123,4081]
	*Lab6> testRndComps 10000 2 
	[91,3913,1105,3367,1541,5611,385,481,703,3325,2465,9,9637,2701]
	
	*Lab6> testRndComps 10000 3 
	[65,1729,949,2465,8911,6601]
	
	When we change the lookup range 0-10000 into 0-100000 (by calling testRndComps 100000 k ), even less false positives are found which indicates that larger composites have a lesser chance of being a false positive.
-}
							

	
{-	===============
	Ex.5 Carmicheal
	
	Fermat's little theorem: 
	b^(p - 1) is equal to b mod p, for any prime p, for all 1 <= b < p
	
	Modular arithmetic congruence: 
	b^n equal to b (mod n) for all 1 < b < n for which b and n are co-prime
	
	These numbers are composites with the same property of modular arithmetic congruence on which Fermat's little theorem is based. Therefore, they should always trigger a false positive when using primeF
-}

--show list of primes in carmichaeal list using Fermat
testCarmF :: Int -> IO()
testCarmF k = printList $ getListF 0 100 k carmichael []

-- usually: returns the first of carmichaels list (294409)
-- but testF' 20 can result in 56052361 (2nd carmichael), disproving my assumption that these numbers are always treated as false positives by Fermat
testF' :: Int -> IO Integer
testF' k = findPrimeF k carmichael

-- match 100 found false positives with first 100 carmichael numbers
compareCarmF :: Int -> IO()
compareCarmF k = do 
				 cms <- getListF 0 100 k carmichael []
				 print $ cms == (take 100 carmichael)
				 
{-
	*Lab6> compareCarm 3
	True
	*Lab6> compareCarm 10	
	False	<-- here some carmichael numbers are not recognized as primes
	*Lab6> compareCarm 10	
	True	<-- here they are
-}

{-
	================
	Ex.6 Miller-Rabin
-}

getListMR :: Int -> Int -> Int -> [Integer] -> [Integer] -> IO [Integer]
getListMR n max k xs ps = if n == max
					      then return $ reverse ps
					      else do 
							   isP <- primeMR k (xs!!n)
							   if isP
							   then getListMR (n+1) max k xs ((xs!!n):ps)
							   else getListMR (n+1) max k xs ps
							   
-- execute MR method with 100 random k's (with range min-1000) on carmicheal
testCarmMR :: Int -> IO()
testCarmMR min = testCarmMR' 100 min

testCarmMR' :: Int -> Int -> IO()
testCarmMR' 0 min = print True
testCarmMR' n min = do
				rnd <- randomRIO (min, 1000)
				cms <- getListMR 0 100 rnd carmichael []
				if cms == []
				then testCarmMR' (n-1) min
				else print ("for k: " ++ (show rnd) ++ ", these primes were found: " ++ (show cms))

{-
	For small k's  Miller Rabin still reports primes on carmicheal numbers
	
	*Lab6> testCarmMR 3
	"for k: 3, these primes were found: [2301745249]"
	*Lab6> testCarmMR 1
	"for k: 1, these primes were found: [172947529,2724933935809,11765530852489,21873528379441,22027380041449,30833142247729,35700127755121,37686301288201,57060521336809,81159260227849,221568419989801]"
	*Lab6> testCarmMR 10
	True
-}			

{- 
	Ex.7 Mersenne
-}

findM :: Int -> IO()
findM n = printList $ findM' n primes []

findM' :: Int -> [Integer] -> [Integer] -> IO [Integer]
findM' 0 ps ms = return $ reverse ms
findM' n (p:ps) ms = do
						isM <- primeMR 10 p'
						if isM
						then findM' (n-1) ps (p':ms)
						else findM' n ps ms
						where p' = (2^p - 1)

{-
	*Lab6> findM 10	[3,7,31,127,8191,131071,524287,2147483647,2305843009213693951,618970019642690137449562111]
-}


{- 
	Ex.8 RSA
	
	unfortunately cannot use IO Bool of primeMR in list comprehension:
	findPairs :: [(Integer, Integer)]
	findPairs = [(x,y) | x <- [10000..12000], y <- [10000..12000], 
						x /= y,
						isPrime x, 
						isPrime y, 
						getBits x == getBits y]
						
	getBits :: Integer -> Integer
	getBits x = floor $ logBase 2 (fromIntegral x) + 1
-}

-- find random pair of primes for given bit-length
findPair :: Int -> IO()
findPair bits = do 
					x <- fst pair
					y <- snd pair
					print $ (x,y)
					where pair = findPair' bits

findPair' :: Int -> (IO Integer, IO Integer)
findPair' bits = (x,y)
					where min = 2^(bits-1); 
						  max = (2^bits)-1;
						  xs = take 1000 [min..max]; --just take 1000 elements, for larger bit-lengths the list becomes too big
						  x = findPrimeInList xs (return 0);
						  y = findPrimeInList xs x;				  

-- we need the second parameter to filter the first prime (if it should accidentally be found)
findPrimeInList :: [Integer] -> IO Integer -> IO Integer
findPrimeInList xs y = do
					r <- randomRIO (0, length xs - 1)
					x <- return (xs!!r)
					p <- primeMR 10 (xs!!r)
					y' <- y
					if p && x /= y'
					then return x
					else findPrimeInList (delete x xs) y

-- show rsa encryption/decryption of message msg with bit-length b
rsaTest :: Int -> Integer -> IO()
rsaTest b msg = do  
				p' <- p
				q' <- q
				pr <- return $ rsa_private p' q'
				pu <- return $ rsa_public p' q'
				enc <- return $ rsa_encode pr msg
				dec <- return $ rsa_decode pu enc
				print $ ("message: " ++ show msg)
				print $ ("primes: " ++ show p' ++ " and " ++ show q')
				print "--------------------------------"
				print $ ("pr_key: " ++ show pr)
				print $ ("pub_key: " ++ show pu)
				print "--------------------------------"
				print $ ("encode message with private: " ++ show enc)
				print $ ("decode encoded message with public: " ++ show dec)
				where 	pq = findPair' b;
						p = fst pq;
						q = snd pq				
						
{-
	64-bit encryption: 
	
	>rsaTest 64 1234
	
	"message: 1234"
	"primes: 9223372036854776561 and 9223372036854776351"
	"--------------------------------"
	"pr_key: (56713727820156418533924711698681570667,85070591730234627819333811621731908911)"
	"pub_key: (3,85070591730234627819333811621731908911)"
	"--------------------------------"
	"encode message with private: 51322416876448132951161862689172134529"
	"decode encoded message with public: 1234"
-}