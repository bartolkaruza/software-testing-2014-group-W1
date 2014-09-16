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
--extract element from triple
eF :: (a, b, c) -> a
eF (a,_,_) = a
eS :: (a, b, c) -> b
eS (_,b,_) = b
eL :: (a, b, c) -> c
eL (_,_,c) = c

--generate rectangular triangles			
genRectTriangles = [ (a,b,c) | c <- [1..100], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]  


--Ex.2: isPermutation function (0.5 hours)
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation (x:xs) ys = 
		if length (map (==x) (x:xs)) == length (map (==x) ys)
		then isPermutation (filter (/=x) xs) (filter (/=x) ys) 
		else False
		
		
--Ex.3 Tests for isPermutation  (1 hour)
testPerm :: ([Int], [Int]) -> Bool
testPerm xs = isPermutation (fst xs) (snd xs)

--the reverse is always a permutation
validPerm :: ([Int], [Int])
validPerm = ([x | x <- [1..100]], [y | y <- reverse [1..100]])
--different lengths
invalidPerm :: ([Int], [Int])
invalidPerm = ([x | x <- [1..100]], [y | y <- [1..99]])
--different start elements
invalidPerm' :: ([Int], [Int])
invalidPerm' = ([x | x <- [1..100]], [y | y <- [2..101]])
-- if the input contains no duplicates, this results in a stronger precondition for the test (stronger reqs on input), so the set of tests will be smaller


--Ex.4: Permutation generation (2 hours)
perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [(x:ys) | x <- xs, ys <- perms (delete x xs)]

--example [1,2,3]:
--x <- xs leads to lists results in lists starting with 1, 2 or 3
--ys <- perms (delete x xs) results to permutations of the remaining list which are then appended to the first list
--all the combinations of first elements and remaining elements result in all the permutations

-- How to test: if n is length of list to be permuted and m is number of resulting permutations then: m = n!
testPerms :: Eq a => [a] -> Bool
testPerms xs = fact (length xs) == length (perms xs)

fact :: Int -> Int
fact 1 = 1
fact n = n * fact(n-1)


--Ex.5: isDerangement function (1 hour)
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = if isPermutation xs ys
					then and (zipWith (/=) xs ys)
					else False

					
--Ex.6: derangement generation (0.5 hours)
deran :: Eq a => [a] -> [[a]]
deran xs = filter (isDerangement xs) (perms xs)


--Ex.7: tests for isDerangement (1 hour)
testDeran :: ([Int], [Int]) -> Bool
testDeran xs = isDerangement (fst xs) (snd xs)
--the reverse is always a derangement
validDeran :: ([Int], [Int])
validDeran = ([x | x <- [1..100]], [y | y <- reverse [1..100]])
--the reverse is always a derangement, but now the first element is not reversed
invalidDeran :: ([Int], [Int])
invalidDeran = ([x | x <- [1..100]], [y | y <- 1:reverse [2..100]])

--Ex.7 continued: testable properties for isDerangements
deranProp1 :: Eq a => [a] -> [a] -> Bool
deranProp1 xs ys = (isDerangement xs ys) == (listsNonEqual xs ys)

listsNonEqual :: Eq a => [a] -> [a] -> Bool
listsNonEqual [] [] = True
listsNonEqual (x:xs) (y:ys) = (x /= y) && (listsNonEqual xs ys)

--Ex.8 Bonus1 (2 hours)
arbDeran :: (Show a, Eq a) => [a] -> IO ()
arbDeran xs = do
	rnd <- getRndIndex (deran xs)
	print $ deran xs!!rnd

getRndIndex :: Eq a => [a] -> IO Int
getRndIndex xs = getStdRandom (randomR (0, length xs - 1))

--Ex. 9 Bonus