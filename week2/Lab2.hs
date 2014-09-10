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
	notriangle' x y z = iets (+) (<=) (||) x y z

	rectangular' :: Integer -> Integer -> Integer -> Bool
	rectangular' x y z = iets pythago (==) (||) x y z
	
	iets :: (Integer -> Integer -> Integer) -> (Integer -> Integer -> Bool) -> (Bool -> Bool -> Bool) -> Integer -> Integer -> Integer -> Bool
	iets o c b x y z = b (b (c (o x y) z) (c (o y z) x)) (c (o x z) y)
	

	-- Ex 2.
	isPermutation :: Eq a => [a] -> [a] -> Bool
	isPermutation [] [] = True
	isPermutation [] _ = False
	isPermutation _ [] = False
	isPermutation (x:xs) ys = isPermutation xs (delete x ys)

	-- Ex 3.

	-- Ex 4.
	perms :: Eq a => [a] -> [[a]]
	perms all@(x:xs) = foldr (\y -> ((x:delete x all)++((delete x all)++[x])):y) [[]] xs
