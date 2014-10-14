module Lab6

where
import Data.List
import System.Random
import Week6
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]
			   
composites :: [Integer]
composites = (take 1000 [2..]) \\ (take 1000 primes)

primeTest_F :: Integer  -> IO Bool
primeTest_F n = do 
   a <- randomRIO (1, n-1) :: IO Integer
   (primeF (fromIntegral a) (n-1))

primeTest_MR ::Integer -> IO Bool
primeTest_MR n = do 
   a <- randomRIO (1, n-1) :: IO Integer
   (primeMR (fromIntegral a) (n-1))
 
-- testF 1 = 4
-- testF 2 = 4
-- testF 3 = 4
-- the higher k, the lower the chance that the test lets the composite number slip. But it still lets it slip sometimes
testF :: Int -> IO ()
testF k = do t <- filterM (\x -> primeF k x) composites
             print t
  
testCM :: Int -> IO ()
testCM k = do t <- filterM (\x -> primeF k x) (take 5 carmichael)
              print t

testCM :: Int -> IO ()
testCM k = do t <- filterM (\x -> primeMR k x) (take 5 carmichael)
              print t


-- mersenne primes are of the form 2^n - 1			  
findMR :: Int -> IO ()
findMR k = do t <- filterM (\p -> primeMR k (2^p - 1)) (take 10 primes)
              print $ map (\p -> 2^p - 1) t		  

{-		QUICKCHECK VARIANT (gets only index of failure)	  
getPrimeTestF x |  x == 0 = getPrimeTestF 1
                | otherwise = monadicIO $ do 
                                  t <- run $ ((primeTest_F (composites !! (abs x))))
                                  assert $ not t

getPrimeTestCM x | x == 0 = getPrimeTestCM 1
                 | otherwise = monadicIO $ do
                       t <- run $ (primeTest_F (carmichael !! (abs x))) 
                       assert $ not t 

getPrimeTestMR x | x == 0 = getPrimeTestMR 1
                 | otherwise =  monadicIO $ do t <- run $ (primeTest_MR (carmichael !! (abs x))) 
                                               assert $ not t 
-}
-- hier een a kiezen met 1 < a < N waarbij N prime en die invullen als eerste argument voor primeF
