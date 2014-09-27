module Lab4

where

import SetOrd
import Data.List
import System.Random
import Test.Hspec
import Test.QuickCheck

{-
	Ex.3 - Random set generation
	(1 hour)
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
	(2 hours)
-}

test1 = Set [1,2,3,4,5,6,7,8]
test2 = Set [6,7,8,9,10,11]

{-
	===================================================
	intersection of set1 and set2: all elements in set1 that are contained in set2
-}
intersectSet :: (Ord a, Eq a) => Set a -> Set a -> Set a
intersectSet (Set []) (Set []) = emptySet
intersectSet (Set []) set2 = emptySet
intersectSet (Set (x:xs)) set2 = if inSet x set2 
							then insertSet x (intersectSet (Set xs) set2) 
							else intersectSet (Set xs) set2
							
--intersection test	
--quickCheck qcIntersectSet --> 100 tests passed!

qcIntersectSet = ((\s t -> isIntersection (list2set s) (list2set t)) :: [Int] -> [Int] -> Bool)
					
isIntersection :: (Ord a, Eq a) => Set a -> Set a -> Bool
isIntersection set1 set2 = (subSet s3 set1) && (subSet s3 set2) && (matchElems set1 set2 s3)
							where s3 = intersectSet set1 set2
							
matchElems :: (Ord a, Eq a) => Set a -> Set a -> Set a -> Bool
matchElems (Set []) (Set []) (Set []) = True
matchElems (Set []) set2 (Set []) = True
matchElems set1 (Set []) (Set []) = True
matchElems (Set [x]) set2 iSet = if (inSet x set2) 
									then (inSet x iSet)
									else True
matchElems (Set (x:xs)) set2 iSet = if (inSet x set2) 
									then (inSet x iSet) && (matchElems (Set xs) set2 iSet) 
									else matchElems (Set xs) set2 iSet
	
	
{-
	===================================================
	difference of set1 and set2: set1 - (set1 intersect set2)						
-}
diffSet :: (Ord a, Eq a) => Set a -> Set a -> Set a
diffSet (Set []) (Set []) = emptySet
diffSet set1 (Set []) = set1
diffSet (Set []) set2 = emptySet
diffSet set1 set2 = removeSet (intersectSet set1 set2) set1

--remove elements contained in set1 from set2
removeSet :: (Ord a, Eq a) => Set a -> Set a -> Set a
removeSet (Set []) set2 = set2
removeSet (Set (x:xs)) set2 = removeSet (Set xs) ( deleteSet x set2)

-- difference test
-- quickCheck qcDiffSet --> 100 tests passed!
qcDiffSet = ((\s t -> isDifferent (diffSet (list2set s) (list2set t)) (list2set t)) :: [Int] -> [Int] -> Bool)

isDifferent :: (Ord a, Eq a) => Set a -> Set a -> Bool
isDifferent (Set []) set2 = True
isDifferent (Set (x:xs)) set2 = (not (inSet x set2)) && ( isDifferent (Set xs) set2)

{-
	Ex.5 Relations
	(1.5 hour)
-}

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]

trClos :: Ord a => Rel a -> Rel a
trClos r = srtRel (r ++ trClos' r r)					

trClos' :: Ord a => Rel a -> Rel a -> Rel a
trClos' r1 [] = []
trClos' r1 r2 = r' ++ (trClos' r1 r')
				where r' = r1 @@ r2

--sort relations by first element
srtRel :: Ord a => Rel a -> Rel a
srtRel r = sortBy (\x y -> compare (fst x) (fst y)) r

test3 = [(1,2),(2,3),(3,4)]
-- trClos test3 : [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

{- 
	Ex.6 Hspec specification
-}

main :: IO ()
main = hspec $ do
	describe "Lab4.trClos" $ do
	  it "returns a list with the given relation when given a list with one relation"$ do
		trClos [(0,1)] `shouldBe` [(0,1)]
		
	  it "returns the transitive closure on the given list of binary relations" $ do
		trClos [(1,2),(2,3),(3,4)] `shouldBe` [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
		
	  --it "returns a empty list when given an empty list"