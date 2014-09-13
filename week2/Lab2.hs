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
					

