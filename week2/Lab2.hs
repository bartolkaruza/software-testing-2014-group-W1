module Lab2 where

	import Data.List
	import System.Random

	data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)

	-- Ex 1.

	triangle :: Integer -> Integer -> Integer -> Shape
	triangle x y z | notriangle x y z = NoTriangle 
			| equilateral x y z = Equilateral 
			| isosceles x y z = Isosceles 
			| rectangular x y z = Rectangular
			| otherwise = Other

	notriangle :: Integer -> Integer -> Integer -> Bool	
	notriangle x y z = x+y <= z || x+z <= y || y+z <= x

	equilateral :: Integer -> Integer -> Integer -> Bool
	equilateral x y z = x == y && y == z

	isosceles :: Integer -> Integer -> Integer -> Bool
	isosceles x y z = x == y || y == z || x == z

	rectangular :: Integer -> Integer -> Integer -> Bool
	rectangular x y z = (pythago x y) == z^2 || (pythago y z) == x^2 || (pythago x z) == y^2

	pythago :: Integer -> Integer -> Integer
	pythago x y = x ^ 2 + y ^ 2

	-- Abstractie experiment
	notriangle' :: Integer -> Integer -> Integer -> Bool
	notriangle' x y z = check (+) (<=) (||) x y z

	rectangular' :: Integer -> Integer -> Integer -> Bool
	rectangular' x y z = check pythago (==) (||) x y z
	
	check :: (Integer -> Integer -> Integer) -> (Integer -> Integer -> Bool) -> (Bool -> Bool -> Bool) -> Integer -> Integer -> Integer -> Bool
	check opp comp bool x y z = bool (bool (comp (opp x y) z) (comp (opp y z) x)) (comp (opp x z) y)
	

	-- Ex 2.
	isPermutation :: Eq a => [a] -> [a] -> Bool
	isPermutation [] [] = True
	isPermutation [] _ = False
	isPermutation _ [] = False
	isPermutation (x:xs) ys = isPermutation xs (delete x ys)

	-- Ex 3.

	-- Ex 4.

	perms :: Eq a => [a] -> [[a]]
	perms [] = [[]]
	perm xs = [x:ys | x <- xs, ys <- perms(delete x xs)]
	-- The number of permutations is the factorial of the number of elements. 
	-- The fact that the elements are unique, means you can use this to remove individual elements from the list, to permute the rest of the list


	-- Ex 5.
	isDerangement :: Eq a => [a] -> [a] -> Bool
	isDerangement [] [] = True
	isDerangement _ [] = False
	isDerangement [] _ = False
	isDerangement (x:xs) (y:ys) = x /= y && (isDerangement xs ys)

	--perms :: Eq a => [a] -> [[a]]
	--perms xs = foldr (\y -> ((x:delete x all)++((delete x all)++[x])):y) [[]] xs


	--permuteElement :: [a] -> Int -> [[a]]
	--permuteElement [] y = [[y]]
	--permuteElement xs y = 
