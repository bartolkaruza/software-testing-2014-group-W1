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
	

-- we can check these outcomes by testing if the list of permutations is of length 2^n where n = length of input
perms :: [a] -> [[a]]
perms [] = []
perms [x] = [[x]]
perms all@(x:xs) =  (concatMap (insert' x)) (perms xs)

insert' :: a -> [a] -> [[a]]
insert' x [] = []
insert' x [y]= [[x,y], [y,x]]
insert' x all@(y:ys) = (x:all):(map (y:) (insert' x ys))

isDerangement ::  [Int] -> [Int] -> Bool

isDerangement xs ys = if isPermutation xs ys then and $ (zipWith (/=) xs ys)
					  else False

deran :: [Int] -> [[Int]]
deran xs = filter (isDerangement xs $) (perms xs)




