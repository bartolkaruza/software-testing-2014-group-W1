module Week4 where

import SetOrd
import System.Random
import Test.QuickCheck
import Data.List
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

getRandomList :: IO [Int]
getRandomList = do
  g <- newStdGen 
  d <- getRandomInt
  return $ take d $ (randomRs (0, 100) g :: [Int])

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


-- I need to check if the set difference of the new relation with the old relation is the empty set
transClosure :: Ord a => Rel a -> Rel a -> Rel a
transClosure ra@(r:rs) sa@(s:ss) = if setDifference (Set (ra @@ sa)) (Set ra) == emptySet && setDifference (Set (ra @@ sa)) (Set sa) == emptySet then setUnion (Set ra) (Set sa)
                                   else transClosure ra (ra @@ sa) 
								   
---- BONUS
{- if we define the value that we put in as v then we can see that the formula is (x+v/x)/2. When x=sqrt(v) then v/x=x, so in that case 
   the output is (x+x)/2=x, which makes the if statement get into the if clause instead of the else clause and the program terminates. 
   As written earlier, in this case x=sqrt(v).
  -}