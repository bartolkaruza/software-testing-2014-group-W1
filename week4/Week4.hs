module Week4 where

import SetOrd
import System.Random
import Test.QuickCheck
import Data.List
import Control.Exception (evaluate)
import Test.Hspec
--gen :: [Int]
--gen = do 
--      g <- newStdGen
--      n <- random g
--      print . tenPseudorandomNumbers n	  


{-
    2. Time: about 1,5 hour and some experimenting 
-}

{- 3. About 4 hours 
-}
getRandomSet :: IO (Set Int)
getRandomSet = do 
               l <- getRandomList
               return ((list2set l) :: Set Int)

-- run test
dupTest = do 
          s <- getRandomSet 
          deepCheck (\x -> checkDuplicates x s) 

-- invariant: no duplicates		  
checkDuplicates :: Int -> Set Int -> Bool
checkDuplicates x s = not (inSet x s) || (inSet x s && not (inSet x (deleteSet x s)))

deepCheck someProp = quickCheckWith stdArgs { maxSuccess = 5000 } someProp
			   
-- helper function
getRandomList :: IO [Int]
getRandomList = do
  g <- newStdGen 
  d <- getRandomInt
  return $ take d $ (randomRs (0, 100) g :: [Int])				   

{- 4. about 15 minutes
-}
setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection s t = setIntersect s t emptySet

setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion s t = joinSets s t emptySet 
 
setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference s t = setDiff s t s

-- helper intersect 
setIntersect :: (Ord a) => Set a -> Set a -> Set a -> Set a
setIntersect s all@(Set (t:ts)) u  = if inSet t s then setIntersect s (Set ts) (insertSet t u)
                                        else setIntersect s (Set ts) u
setIntersect _ _ u = u

-- helper union
joinSets :: (Ord a) => Set a -> Set a -> Set a -> Set a
joinSets s all@(Set (t:ts)) u  = joinSets s (Set ts) (insertSet t u)
joinSets all@(Set (s:ss)) ts@emptySet u  = joinSets (Set ss) ts (insertSet s u)
joinSets _ _ u = u

-- helper diff	  
setDiff :: (Ord a) => Set a -> Set a -> Set a -> Set a 				 
setDiff s (Set (t:ts)) u = setDiff s (Set ts) (deleteSet t u)
setDiff _ _ u = u

{- 5. about 1.5 hours
-}

type Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos r = transClosure r r 
trClos [] = error "can't be used with an empty list"

transClosure :: Ord a => Rel a -> Rel a -> Rel a
transClosure ra sa = if setDifference (setUnion (Set ra) (Set (ra @@ sa) )) (setUnion (Set sa) (Set ra) ) == emptySet then toRel (setUnion (Set ra) (Set (ra @@ sa) ))
                     else transClosure ra $ toRel (setUnion (Set ra) (Set (ra @@ sa)))

{- 6. about 2 hours
    Can be called with hspecDef
	- this function uses examples to define the trClos function
-}

hspecDef :: IO ()
trClosSpec = hspec mySpec

mySpec = describe "trClos" $ do
    it "returns the transitive closure of a relation" $ do
       trClos [(1,2),(2,3),(3,4)] `shouldBe` [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
	 
    it "contains at least all the elements of input r" $ do
       let r = [(1,2),(2,3),(3,4)]
       all (\x -> (elem x (trClos r))) r
	 
    it "returns a relation that is transitive" $ do
       let r = trClos [(1,2),(2,3),(3,4)]
       all (==True) [(elem (x,z) r) | (x,y) <- r,  (w,z) <- r, y == w]

{- 7. About 30 minutes 
   Can be called using qcDef
   - this function defines trClos using QuickCheck properties
-}
qcDef :: IO ()
qcDef = hspec mySpecQC

mySpecQC = describe "trClos" $ do
    it "returns the transitive closure of a relation" $ do
       trClos [(1,2),(2,3),(3,4)] `shouldBe` [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
	 
    it "contains at least all the elements of input r" $ 
       property $ checkBaseRelation
           
	 
    it "returns a relation that is transitive" $ do
       property $ checkTransitivity   
	   
checkBaseRelation r s = let 
                         t = (zip r s) 
                      in all (\x -> (elem x (trClos t))) t
   where types = (r :: [Int], s :: [Int])
	
checkTransitivity r s = let 
                         t = trClos (zip r s) 
                      in all (==True) [(elem (x,z) t) | (x,y) <- t,  (w,z) <- t, y == w]
   where types = (r :: [Int], s :: [Int])



toTuples :: [a] -> [a] -> [(a,a)]
toTuples [] [] = []
toTuples [] ys = error "lists are not of the same size"
toTuples xs [] = error "lists are not of the same size"  
toTuples (x:xs) (y:ys) = (x,y) : (toTuples xs ys)

		
getRandomTupleList :: IO [(Int,Int)]
getRandomTupleList = do
  g <- newStdGen 
  d <- getRandomInt
  let xs = take d $ (randoms g :: [Int])
  let ys = take d $ (randoms g :: [Int])
  return $ toTuples xs ys

getRandomInt = do
  g <- newStdGen 
  let [n] = take 1 $ (randomRs (0, 100) g :: [Int])
  return n

					 

			   
--getRandomRelation :: IO (Rel a)
--getRandomRelation = do 
--               l <- getRandomTupleList :: [(a, a)]
--               return $ list2set l		   


  


	
toRel :: Set (a, a) -> Rel a
toRel (Set yall@(y:ys)) = yall	
toRel emptySet = []
						   
---- BONUS
{- if we define the value that we put in as v then we can see that the formula is (x+v/x)/2. When x=sqrt(v) then v/x=x, so in that case 
   the output is (x+x)/2=x, which makes the if statement get into the if clause instead of the else clause and the program terminates. 
   As written earlier, in this case x=sqrt(v).
  -}