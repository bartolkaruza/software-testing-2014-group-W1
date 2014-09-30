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

-- invariant: no duplicates
dupTest = do 
          s <- getRandomSet 
          deepCheck (\x -> checkDuplicates x s) 
 
checkDuplicates :: Int -> Set Int -> Bool
checkDuplicates x s = not (inSet x s) || (inSet x s && not (inSet x (deleteSet x s)))

deepCheck someProp = quickCheckWith stdArgs { maxSuccess = 5000 } someProp

getRandomSet :: IO (Set Int)
getRandomSet = do 
               l <- getRandomList
               return ((list2set l) :: Set Int)
			   
--getRandomRelation :: IO (Rel a)
--getRandomRelation = do 
--               l <- getRandomTupleList :: [(a, a)]
--               return $ list2set l

getRandomList :: IO [Int]
getRandomList = do
  g <- newStdGen 
  d <- getRandomInt
  return $ take d $ (randomRs (0, 100) g :: [Int])			   

toTuples :: [a] -> [a] -> [(a,a)]
toTuples [] [] = []
toTuples [] ys = error "lists are not of the same size"
toTuples xs [] = error "lists are not of the same size"  
toTuples (x:xs) (y:ys) = (x,y) : (toTuples xs ys)

		
getRandomTupleList :: IO [(Int,Int)]
getRandomTupleList = do
  g <- newStdGen 
  let xs = take 10 $ (randoms g :: [Int])
  let ys = take 10 $ (randoms g :: [Int])
  return $ toTuples xs ys

getRandomInt = do
  g <- newStdGen 
  let [n] = take 1 $ (randomRs (0, 100) g :: [Int])
  return n
  
setIntersection s t = setIntersect s t emptySet

setUnion s t = joinSets s t emptySet 
 
setDifference s t = setDiff s t s

-- helper intersect 
setIntersect s all@(Set (t:ts)) u  = if inSet t s then setIntersect s (Set ts) (insertSet t u)
                                        else setIntersect s (Set ts) u
setIntersect _ _ u = u

-- helper union
joinSets s all@(Set (t:ts)) u  = joinSets s (Set ts) (insertSet t u)
joinSets all@(Set (s:ss)) ts@emptySet u  = joinSets (Set ss) ts (insertSet s u)
joinSets _ _ u = u

-- helper diff	   				 
setDiff s (Set (t:ts)) u = setDiff s (Set ts) (deleteSet t u)
setDiff _ _ u = u

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
 

trClosSpec :: IO ()
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

mySpecQC = describe "trClos" $ do
    it "returns the transitive closure of a relation" $ do
       trClos [(1,2),(2,3),(3,4)] `shouldBe` [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
	 
    it "contains at least all the elements of input r" $ 
       property $ checkBaseRelation
           
	 
    it "returns a relation that is transitive" $ do
       property $ checkTransitivity   
	   
checkBaseRelation r = all (\x -> (elem x (trClos r))) r
   where types = r :: [(Int, Int)]
	
checkTransitivity r s = let 
                         t = (zip r s) 
                      in all (==True) [(elem (x,z) t) | (x,y) <- t,  (w,z) <- t, y == w]
   where types = (r :: [Int], s :: [Int])

	
toRel :: Set (a, a) -> Rel a
toRel (Set yall@(y:ys)) = yall	
toRel emptySet = []
						   
---- BONUS
{- if we define the value that we put in as v then we can see that the formula is (x+v/x)/2. When x=sqrt(v) then v/x=x, so in that case 
   the output is (x+x)/2=x, which makes the if statement get into the if clause instead of the else clause and the program terminates. 
   As written earlier, in this case x=sqrt(v).
  -}