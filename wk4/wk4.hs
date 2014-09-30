module Lab4

where

import SetOrd
import Data.List
import System.Random
import Test.QuickCheck
import Test.Hspec

-- 3: 1.0 hr

getRndSet :: Int -> Int -> IO ()
getRndSet n r = do d <- getStdGen
                   print $ list2set $ take n (randomRs (0, r) d :: [Int]) 


qctest = quickCheck ((\s -> list2set s == list2set s) :: [Int] -> Bool)


-- 4 1.0 hr

setIntersect :: (Ord a) => Set a -> Set a -> Set a
setIntersect (Set a) (Set b) = list2set $ filter (\x -> elem x b) a

-- simple version
setIntersectSimple :: (Ord a) => Set a -> Set a -> Set a
setIntersectSimple (Set a) (Set b) = list2set $ intersect a b

testIntersectQC :: IO ()
testIntersectQC =  quickCheck ((\s l -> subSet (setIntersect (list2set s) (list2set l) ) (list2set s) ) :: [Int] -> [Int] -> Bool)

-- intersection == intersection of self
testIntersectRG :: Int -> Int -> IO ()
testIntersectRG n r = do d <- getStdGen
                         let a = list2set $ take n (randomRs (0, r) d :: [Int]) in 
                            print $ a == setIntersect a a 

-- union given by SetOrd.hs

-- simple version
setUnionSimple :: (Ord a) => Set a -> Set a -> Set a
setUnionSimple (Set a) (Set b) = list2set $ union a b

setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference (Set a) (Set b) = list2set $ filter (\x -> not $ elem x b) a

-- simple version
setDifferenceSimple :: (Ord a) => Set a -> Set a -> Set a
setDifferenceSimple (Set a) (Set b) = list2set $ a \\ b

-- 5 1.0hr

type Rel a = [(a, a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w]

trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos (a:xs) = [a] ++ trClos ( trClos ([a] @@ xs) ++ xs)

-- 6 0.5hr

trClosSpec = hspec $ do
    describe "transitiveClosure" $ do
        it "returns transitive closure tuples given a list of tuples" $ 
            trClos [(1,2),(2,3)] `shouldBe` [(1,2),(1,3),(2,3)]
