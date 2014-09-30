module Lab4

where

import SetOrd
import Data.List
import System.Random
import Test.Hspec
import Test.QuickCheck

-- run all using
runAll = do
           runallTestEx4
           runallTestEx5
           main
           runallTestEx7


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

-- please use this to check all set operations
runallTestEx4 = do 
         print "run test intersect"
         quickCheck qcIntersectSet
         print "run test difference"
         quickCheck qcDiffSet 
         print "run test union"
         quickCheck qcUnionSet

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
 unionSet is already defined in the SetOrd, so we skipped the implementation. We have implemented the following QuickCheck test
-}

qcUnionSet = (\s t -> (and [((inSet x (Set s)) && (inSet x (Set t)) && (inSet x (unionSet (Set s) (Set t)))) || not ((inSet x (Set s)) && (inSet x (Set t)))  | x <- s, y <- t ]) ) :: [Int] -> [Int] -> Bool



{-
	Ex.5 Relations
	(1.5 hour)
-}

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]

trClos :: Ord a => Rel a -> Rel a
trClos r = transClosure r r 

-- uses the property that the union of all the (ra @@ sa) should give a transitive closure, and that the transitive closure has been reached
-- when no other element is added to the relation
transClosure :: Ord a => Rel a -> Rel a -> Rel a
transClosure [] [] = []
transClosure ra sa | (length ra) /= (length sa) =  error "list of unequal length"
                   | otherwise =  if diffSet (Set t) (unionSet (Set sa) (Set ra) ) == emptySet 
                                  then t
                                  else transClosure t t
                                     where r' = ra @@ sa 
                                           Set t = (unionSet (Set ra) (Set r'))

toRel :: Set (a, a) -> [(a, a)]
toRel (Set xs) = xs

--sort relations by first element
srtRel :: Ord a => Rel a -> Rel a
srtRel r = sortBy (\x y -> compare (fst x) (fst y)) r

test3 = [(1,2),(2,3),(3,4)]

runallTestEx5 = do 
         print "Transitive closure of [(1,2),(2,3),(3,4)]"
         print $ trClos test3

-- trClos test3 : [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

{- 
	Ex.6 Hspec specification
	(0.5 hour)
-}

main :: IO ()
main = hspec $ do
    describe "Lab4.trClos" $ do
      it "returns a list with the given relation when given a list with one relation" $
        trClos [(0,1)] `shouldBe` [(0,1)]
		
      it "returns a relation that is transitive" $ do
         let r = trClos [(1,2),(2,3),(3,4)]
         isTransitive (trClos r)
		 
      it "returns the transitive closure on the given list of binary relations" $
        trClos [(1,2),(2,3),(3,4)] `shouldBe` [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
	  --it "returns a empty list when given an empty list"
	  
{-
	Ex.7 Testing trClos
	(1.5 hours)
-}

-- run with this
runallTestEx7 = do
                 print "using quickcheck" 
                 quickCheck qcTrClos
                 print "using own random generator"
                 testTrClos

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
getRandomR g = zip (getRndList 100 100 g) (getRndList 100 100 g)

--quickCheck function 
qcTrClos = ((\z -> testClosureTr z) :: [(Int, Int)] -> Bool )

---- BONUS (5 min)
{- if we define the value that we put in as v then we can see that the formula is (x+v/x)/2. When x=sqrt(v) then v/x=x, so in that case 
   the output is (x+x)/2=x, which makes the if statement get into the if clause instead of the else clause and the program terminates. 
   As written earlier, in this case x=sqrt(v).
  -}