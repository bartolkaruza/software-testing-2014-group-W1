module Main where

import Lab4
import SetOrd
import Data.List
import System.Random
import Test.Hspec
import Test.QuickCheck

{-
	Ex.3 - Random set generation
-}
getRandomSet :: IO ()
getRandomSet = do 
	gen <- newStdGen
	print $ list2set (getRndList 100 gen)
	
getRndList :: Int -> StdGen -> [Int]
getRndList n g = take n (randomRs (0, 1000) g :: [Int])

qcSetContents = verboseCheck ((\s -> list2set s == list2set s) :: [Int] -> Bool)

--qcSetUnique = verboseCheck ((\s -> (length ( nub ( list2set s))) == (length (list2set s))) :: [Int] -> Bool)

{-
	Ex.4 Set operations
-}

test1 = Set [1,2,3,4,5]
test2 = Set [1,0,6,7,4]

intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet (Set []) (Set []) = Set []
intersectSet set1 (Set []) = Set []
intersectSet (Set []) set1 = Set []
intersectSet set1 set2 = unionSet (getSubset set1 set2) (getSubset set2 set1)

getSubset :: (Ord a) => Set a -> Set a -> Set a
getSubset (Set []) set2 = Set []
getSubset (Set (x:xs)) set2 = if inSet x set2 
							then insertSet x (getSubset (Set xs) set2) 
							else getSubset (Set xs) set2
							
							
qcSetIntersect = ((\s t -> isIntersection (list2set s) (list2set t)) :: [Int] -> [Int] -> Bool)
					
isIntersection :: (Ord a) => Set a -> Set a -> Bool
isIntersection set1 set2 = (subSet s3 set1) && (subSet s3 set2)
							where s3 = intersectSet set1 set2