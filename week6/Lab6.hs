module Lab6

where
import Data.List
import System.Random
import Week6
import Test.QuickCheck
import Test.QuickCheck.Monadic

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]

powerMod :: Int -> Int -> Int -> Int
powerMod x 0 m = 1 
powerMod x n m | even n =  (mod (powerMod x (n `div` 2) m) m) * (mod (powerMod x (n `div` 2) m) m)
               | odd n  =  (powerMod x (n-1) m) * (mod x m) 
			   
composites :: [Integer]
composites = [2..] \\ primes

checkPrimeF = verboseCheck getPrimeTestF

primeTest_F :: Integer  -> IO Bool
primeTest_F n = do 
   a <- randomRIO (1, n-1) :: IO Integer
   print a
   (primeF (fromIntegral a) (n-1))

primeTest_MR ::Integer -> IO Bool
primeTest_MR n = do 
   a <- randomRIO (1, n-1) :: IO Integer
   (primeMR (fromIntegral a) (n-1))
  

getPrimeTestF x | x < 0 = getPrimeTestF (-x)
                | x == 0 = getPrimeTestF 1
                | otherwise = monadicIO $ do 
                                  t <- run $ ((primeTest_F (composites !! x)))
                                  assert $ not t

getPrimeTestCM x | x < 0 = getPrimeTestCM (-x)
                 | x == 0 = getPrimeTestCM 1
                 | otherwise = monadicIO $ do
                       t <- run $ (primeTest_F (carmichael !! x)) 
                       assert $ not t 

getPrimeTestMR x | x < 0 = getPrimeTestMR (-x)
                 | x == 0 = getPrimeTestMR 1
                 | otherwise =  monadicIO $ do t <- run $ (primeTest_MR (carmichael !! x)) 
                                               assert $ not t 
-- hier een a kiezen met 1 < a < N waarbij N prime en die invullen als eerste argument voor primeF
