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

test1 = Set [1,2,3,4,5,6,7,8]
test2 = Set [6,7,8,9,10,11]

--intersection of set1 and set2: all elements in set1 that are contained in set2
intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet (Set []) (Set []) = Set []
intersectSet set1 (Set []) = Set []
intersectSet (Set []) set2 = Set []
intersectSet (Set (x:xs)) set2 = if inSet x set2 
							then insertSet x (intersectSet (Set xs) set2) 
							else intersectSet (Set xs) set2
						
--difference of set1 and set2: set1 - (set1 intersect set2)						
diffSet :: (Eq a, Ord a) => Set a -> Set a -> Set a
diffSet (Set []) (Set []) = Set []
diffSet set1 (Set []) = set1
diffSet (Set []) set2 = set2
diffSet set1 set2 = filterSet (intersectSet set1 set2) set1

--remove elements contained in set1 from set2
filterSet :: (Eq a, Ord a) => Set a -> Set a -> Set a
filterSet (Set []) set2 = set2
filterSet (Set (x:xs)) set2 = filterSet (Set xs) ( deleteSet x set2)
							
qcIntersectSet = ((\s t -> isIntersection (list2set s) (list2set t)) :: [Int] -> [Int] -> Bool)
					
isIntersection :: (Ord a) => Set a -> Set a -> Bool
isIntersection set1 set2 = (subSet s3 set1) && (subSet s3 set2)
							where s3 = intersectSet set1 set2