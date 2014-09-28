module Week4 where

import SetOrd
import System.Random
import Test.QuickCheck
--gen :: [Int]
--gen = do 
--      g <- newStdGen
--      n <- random g
--      print . tenPseudorandomNumbers n	  

-- invariant: no duplicates
checkDuplicates = quickCheck (\x ->  do s <- getRandomSet
                                     not (inSet x s) || (inSet x s && not (inSet x (deleteSet x s))))

getRandomSet :: IO (Set Int)
getRandomSet = do 
               l <- getRandomList
               return ((list2set l) :: Set Int)

getRandomList :: IO [Int]
getRandomList = do
  g <- newStdGen 
  d <- getRandomInt
  return $ take d $ (randomRs (0, 100) g :: [Int])

getRandomInt = do
  g <- newStdGen 
  let [n] = take 1 $ (randomRs (0, 100) g :: [Int])
  return n
  
--getRandomIntList :: Int -> IO [Int]
--getRandomIntList n = do
--  g <- getStdGen
--  take n (randoms g :: Int)

--getRandomSet = do 
--               let xs = test
--               print $ list2set xs 
				   
				 
