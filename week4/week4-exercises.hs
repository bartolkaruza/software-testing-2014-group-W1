module Lab4

where

import SetOrd
import Data.List
import System.Random
import Test.Hspec
import Test.QuickCheck

{-
	Ex.3 - Random set generation
	(0.5 hour)
-}
getRandomSet :: IO ()
getRandomSet = do 
	gen <- newStdGen
	print $ getRndSet 100 1000 gen

getRndSet :: Int -> Int -> StdGen -> Set Int
getRndSet n r g = list2set $ getRndList n r g

-- this function is also used in Ex.7
getRndList :: Int -> Int -> StdGen -> [Int]
getRndList n r g = take n (randomRs (0, r) g :: [Int])

qcEqualSets = ((\s -> list2set s == list2set s) :: [Int] -> Bool)
qcUniqueElems = ((\s -> uniqueList $ list2set s):: [Int] -> Bool)

uniqueList :: Eq a => Set a -> Bool
uniqueList (Set []) = True
uniqueList (Set xs) = length (nub xs) == length xs  

--quickCheck qcEqualSets --> 100 tests passed!
--quickCheck qcUniqueElems --> 100 tests passed!

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
intersectSet (Set []) _ = emptySet
intersectSet (Set (x:xs)) set2 = if inSet x set2 
							then insertSet x (intersectSet (Set xs) set2) 
							else intersectSet (Set xs) set2
							
--intersection test	
--quickCheck qcIntersectSet --> 100 tests passed!

qcIntersectSet = ((\s t -> isIntersection (list2set s) (list2set t)) :: [Int] -> [Int] -> Bool)
					
isIntersection :: (Ord a, Eq a) => Set a -> Set a -> Bool
isIntersection set1 set2 = (subSet s3 set1) && 
						   (subSet s3 set2) && 
						   (matchElems set1 set2 s3)
							where s3 = intersectSet set1 set2
							
matchElems :: (Ord a, Eq a) => Set a -> Set a -> Set a -> Bool
matchElems _ _ (Set []) = True
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
diffSet (Set []) _ = emptySet
diffSet set1 (Set []) = set1
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
trClos r = trClos' $ srtRel r

--clean implementation (Bartol), but it seems that quickCheck can generate relations that do not bottom out in the @@ function.
trClos' :: Ord a => Rel a -> Rel a
trClos' [] = []
trClos' (x:xs) = x : trClos (trClos ([x]@@xs) ++ xs)

{-
--this version works on any input (Axel), however it uses nub after processing to deal with initial duplicates.
trClos :: Ord a => Rel a -> Rel a
trClos r = nub (srtRel (r ++ trClos' r r))

--regarding the if-check: the given r2 should not contain elements also present in r', this means the @@ function is bottomed out (in some sense), this can happen with symmetrical relations
trClos' :: Ord a => Rel a -> Rel a -> Rel a
trClos' r1 [] = []
trClos' r1 r2 = if((length [ r | r <- r2, not (elem r r') ] > 0)) 
				then r' ++ (trClos' r1 r')
				else []
				where r' = r1 @@ r2
-}

--sort relations by first element
srtRel :: Ord a => Rel a -> Rel a
srtRel r = sortBy (\x y -> compare (fst x) (fst y)) r

test3 = [(1,2),(2,3),(3,4)]
-- trClos test3 : [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

{- 
	Ex.6 Hspec specification
	(0.5 hour)
-}

main :: IO ()
main = hspec $ do
	describe "Lab4.trClos" $ do
	  it "returns a list with the given relation when given a list with one relation"$ do
		trClos [(0,1)] `shouldBe` [(0,1)]
		
	  it "returns the transitive closure on the given list of binary relations" $ do
		trClos [(1,2),(2,3),(3,4)] `shouldBe` [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
		
	  --it "returns a empty list when given an empty list"
	  
{-
	Ex.7 Testing trClos
	(1.5 hours)
-}

-- for each pair in r, check if it is transitive with any other pair it should be transitive with
isTransitive :: Ord a => Rel a -> Bool
isTransitive [] = True
isTransitive r = and [ hasTrans (t,s) r | (t,s) <- r ]

-- for (x,y) in r match on (u,v) in r so that y == u, then look for an (x,v)
hasTrans :: Ord a => (a,a) -> Rel a -> Bool
hasTrans (x,y) r = and [ elem (x,v) r | (u,v) <- r, y == u]

-- check if R is subset of trClos of R
subRel :: Ord a => Rel a -> Rel a -> Bool
subRel set1 set2 = and [ elem (x,y) set2 | (x,y) <- set1 ]

-- main test function
testClosureTr :: Ord a => Rel a -> Bool
testClosureTr r = isTransitive r' && subRel r r'
				where r' = trClos r

-- own implementation: random relations
testTrClos :: IO()
testTrClos = do 
	gen <- newStdGen
	print $ testClosureTr $ getRandomR gen

getRandomR :: StdGen -> [(Int, Int)]
getRandomR g = zip (getRndList 100 20 g) (getRndList 100 20 g)

--quickCheck function (generate two Int lists, zip them to get a Int relation [(Int, Int)]
qcTrClos = ((\x y -> testClosureTr (zip x y)) :: [Int] -> [Int] -> Bool)