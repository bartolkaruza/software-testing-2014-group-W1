module Lab2 where 

import Data.List
import System.Random

---------------
-- assignment 1
-- 20 minutes
data Shape = NoTriangle 
			| Equilateral
			| Isosceles 
			| Rectangular 
			| Other 
			deriving (Eq,Show)


triangle :: Int -> Int -> Int -> Shape
triangle a b c | a <= 0 || b <= 0 || c <= 0 = NoTriangle
			   | (a == b) && (b == c) = Equilateral
			   | (a == b || b == c || a == c) = Isosceles
			   | a^2 + b^2 == c^2 = Rectangular
			   | a + b >= c || b + c >= a || a + c >= b = Other
			   | otherwise = NoTriangle

			   
---------------
-- assignment 2
-- 30 minutes
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs [] = False
isPermutation [] ys = False
isPermutation all@(x:xs) ys = if ((length all) /= (length ys)) then False
							  else isPermutation xs (delete x ys)
	
-------------------
-- helper functions
infix 1 ==>

(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y

infix 1 <=>

(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y 


---------------------------
-- assignment 3 (property 1)
-- 2 minutes
-- we can check that lists with unequal length are not permutations
-- test using [1,2,3,4] [1,2,3,4,5]
-- test using [1,2,3,4,5] [1,2,3,4]
-- test using [1,2,3,4,5] [5,4,3,2,1]
lengthEqual :: Eq a => [a] -> [a] -> Bool
lengthEqual = (\x y -> ((length x) /= (length y)) ==> not (isPermutation x y) )

-- assignment 3 (property 2)
-- 2 minutes
-- we can check that if a is a permutation of b then b is a permutation of a
-- test using [1,2,3,4,5] [3,4,2,1,5]
reflexivePermutation :: Eq a => [a] -> [a] -> Bool
reflexivePermutation = (\x y -> isPermutation y x <=> isPermutation x y)

-- assignment 3 (property 3)
-- 2 minutes
-- we can check that lists that are the same are permutations
equalityPermutation :: Eq a => [a] -> [a] -> Bool
equalityPermutation = (\x y -> (x == y) ==> isPermutation x y)


-- assignment 3 (property 4)
-- 2 minutes
-- we can check that if a is a permutation of b and b is a permutation of c then a is a permutation of c
-- test using commutativePermutation [1,2,3,4,5] [5,4,3,2,1] [3,2,1,5,4]
commutativePermutation :: Eq a => [a] -> [a] -> [a] -> Bool
commutativePermutation = (\ p q r -> ((isPermutation p q) && (isPermutation q r)) ==> isPermutation p r)


---------------
-- assignment 4
-- 1 hour
-- we can check these outcomes by testing if the list of permutations is of length n! where n = length of input
perms :: [a] -> [[a]]
perms [] = error "empty list"
perms [x] = [[x]]
perms all@(x:xs) =  (concatMap (insert' x)) (perms xs)


insert' :: a -> [a] -> [[a]]
insert' x [] = []
insert' x [y]= [[x,y], [y,x]]
insert' x all@(y:ys) = (x:all):(map (y:) (insert' x ys))


--------------------------------------
--Ex.5: isDerangement function (20 minutes)
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = if isPermutation xs ys
					then and (zipWith (/=) xs ys)
					else False


-----------------------------------------					
--Ex.6: derangement generation (10 minutes)
deran :: Eq a => [a] -> [[a]]
deran xs = filter (isDerangement xs) (perms xs)


---------------------------------------
--Ex.7: tests for isDerangement (1 hour)
--isDerangement implies that elements are not equal
deranTest1 :: Eq a => [a] -> [a] -> Bool
deranTest1 xs ys = (isDerangement xs ys) ==> (listsNonEqual xs ys)

listsNonEqual :: Eq a => [a] -> [a] -> Bool
listsNonEqual [] [] = True
listsNonEqual (x:xs) (y:ys) = (x /= y) && (listsNonEqual xs ys)

--test execution function
execTest :: ([Int], [Int]) -> Bool
execTest xs = deranTest1 (fst xs) (snd xs)

--test cases
validDeran :: ([Int], [Int])
validDeran = ([x | x <- [1..100]], [y | y <- reverse [1..100]])
invalidDeran :: ([Int], [Int])
invalidDeran = ([x | x <- [1..100]], [y | y <- 1:reverse [2..100]])

--execTest validDeran leads to True
--exexTest invalidDeran leads to True

	
----------------------
--Ex.8 Bonus1 (2 hours)
arbDeran :: (Show a, Eq a) => [a] -> IO ()
arbDeran xs = do
	rnd <- getRndIndex (deran xs)
	print $ getRndDeran xs rnd

-- get random index from list of derangements
getRndIndex :: Eq a => [a] -> IO Int
getRndIndex xs = getStdRandom (randomR (0, length xs - 1))

-- get derangement from list based on (random) index
getRndDeran :: Eq a => [a] -> Int -> [a]
getRndDeran xs index = (deran xs)!!index

-- deranTest1 does not show that the functions works, because it only shows that the isPermutaton function is correct
-- tests for arbDeran should show that the permutation was picked randomly(which is difficult) and that the the resulting list actually is a permutation