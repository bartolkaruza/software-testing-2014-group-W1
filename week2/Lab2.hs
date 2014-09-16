module Lab2 where 

import Data.List
import System.Random

data Shape = NoTriangle 
			| Equilateral
			| Isosceles 
			| Rectangular 
			| Other 
			deriving (Eq,Show)


triangle :: Int -> Int -> Int -> Shape
triangle a b c | (a == b || b == c || a == c) = Isosceles
			   | (a == b) && (b == c) = Equilateral
			   | a^2 + b^2 == c^2 = Rectangular
			   | a + b >= c || b + c >= a || a + c >= b = Other
			   | otherwise = NoTriangle

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation xs [] = False
isPermutation [] ys = False
isPermutation all@(x:xs) ys = if ((length all) /= (length ys)) then False
							  else isPermutation xs (delete x ys)
	
-- we can check that lists with unequal length are not permutations
-- we can check that lists that are the same are permutations
-- we can check that if a is a permutation of b then b is a permutation of a
-- we can check that if a is a permutation of b and b is a permutation of c then a is a permutation of c




perms :: [a] -> [[a]]
perms [] = error "empty list"
perms [x] = [[x]]
perms all@(x:xs) =  (concatMap (insert' x)) (perms xs)

-- we can check these outcomes by testing if the list of permutations is of length n! where n = length of input

insert' :: a -> [a] -> [[a]]
insert' x [] = []
insert' x [y]= [[x,y], [y,x]]
insert' x all@(y:ys) = (x:all):(map (y:) (insert' x ys))

isDerangement ::  [Int] -> [Int] -> Bool
isDerangement xs ys = if isPermutation xs ys then and $ (zipWith (/=) xs ys)
					  else False
			  
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
			


