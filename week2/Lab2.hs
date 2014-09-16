module Lab2 where 

import Data.List
import System.Random

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

-- assignment 2
-- 30 minutes
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs [] = False
isPermutation [] ys = False
isPermutation all@(x:xs) ys = if ((length all) /= (length ys)) then False
							  else isPermutation xs (delete x ys)
	
-- helper functions
infix 1 ==>

(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y

infix 1 <=>

(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y 


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


-- assignment 5
-- 5 minutes
isDerangement ::  [Int] -> [Int] -> Bool
isDerangement xs ys = if isPermutation xs ys then and $ (zipWith (/=) xs ys)
					  else False

-- assignment 6		
-- 5 minutes			  
-- here we need to do something to make sure that the input is a natural number
deran :: [Int] -> [[Int]]
deran xs = filter (isDerangement xs $) (perms xs)

-- we could test if there are duplicates in the list

-- we could test if the count is the same
-- we could test if each element in the old list is in the new list
-- Lists:
-- [1,2,3,4,5]
-- [1,1,2,3,4]
-- [1,4,5,6,7]
-- [-1,2,3,4,5]
-- [,1,2,3,4]

--rndDeran :: [Int] -> [Int]
--rndDeran xs = (deran xs) !! randomNr
--			where randomNr = randoms (2^(length xs)) :: Int
			


