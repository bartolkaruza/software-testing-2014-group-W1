module Lab2 where 

import Data.List
import System.Random

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)

--Ex.1: 2 hours
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | a == b && a == c = Equilateral
			   | a == c || a == b || b == c = Isosceles
			   | a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == a^2 = Rectangular
			   | a + b < c || a + c < b || b + c < a = NoTriangle
			   | otherwise = Other
		
-- Simple tests		
-- triangle 10 9 10			Isosceles
-- triangle 10 10 10		Equilateral
-- triangle 5 4 3			Rectangle
-- triangle 5 4 18			NoTriangle
-- triangle 5 4 6 			Other

-- generic test function
triangleTest :: [(Integer, Integer, Integer)] -> Shape -> Bool
triangleTest [] s = True
triangleTest (x:xs) s = (triangle (eF x) (eS x) (eL x) == s) && triangleTest xs s

--generate rectangular triangles			
genRectTriangles = [ (a,b,c) | c <- [1..100], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]  

--extract element from triple
eF :: (a, b, c) -> a
eF (a,_,_) = a
eS :: (a, b, c) -> b
eS (_,b,_) = b
eL :: (a, b, c) -> c
eL (_,_,c) = c


--Ex.2
-- a does not derive 'Ord a', otherwise we could compare the sorted lists isPermutation xs ys = (sort xs) == (sort ys)
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation (x:xs) ys = 
		if length (map (==x) (x:xs)) == length (map (==x) ys)
		then isPermutation (filter (/=x) xs) (filter (/=x) ys) 
		else False
		
--Ex.3 Testable properties??
testPerm :: ([Int], [Int]) -> Bool
testPerm xs = isPermutation (fst xs) (snd xs)

validPerm :: ([Int], [Int])
validPerm = ([x | x <- [1..100]], [y | y <- reverse [1..100]])

invalidPerm :: ([Int], [Int])
invalidPerm = ([x | x <- [1..100]], [y | y <- [101..200]])

--Ex.4
perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [(x:ys) | x <- xs, ys <- perms (delete x xs)]
--example [1,2,3] 
--x <- xs leads to lists results in lists starting with 1, 2 or 3
--ys <- perms (delete x xs) results to permutations of the remaining list which are then appended to the first list
--all the combinations of first elements and remaining elements result in all the permutations

-- How to test: if n is length of list to be permuted and m is number of resulting permutations then: m = n!
testPerms :: Eq a => [a] -> Bool
testPerms xs = fact (length xs) == length (perms xs)

fact :: Int -> Int
fact 1 = 1
fact n = n * fact(n-1)

--Ex. 5
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = if isPermutation xs ys
					then and (zipWith (/=) xs ys)
					else False

--Ex. 6
deran :: Eq a => [a] -> [[a]]
deran xs = filter (isDerangement xs) (perms xs)

--Ex. 7 Testable properties