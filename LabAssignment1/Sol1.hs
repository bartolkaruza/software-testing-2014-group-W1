module Sol1
where

import GS

--ex1.1
ex1a = 3*7
ex1b = 3-7
ex1c = 7/3
ex1d = 7^3

--ex1.4
-- no difference, because...

--ex1.5
ex15a = prime0 0
ex15b = prime0 1
ex15c = prime0 9
ex15d = prime0 17

--ex1.6
--rem :: Int -> Int -> Int

--ex1.7
-- :t divides 5 represents the type of the 'second' procedure that results from the call, namely one with argument Int and result Bool
-- :t divides 5 7 represents the return type of the result of the second procedure

--ex1.9
mnmMax :: [Int] -> Int
mnmMax [] = error "cannot determine max on an empty list!"
mnmMax [x] = x
mnmMax (x:xs) = max x (mnmMax xs)

--ex1.10
removeFst :: [Int] -> Int -> [Int]
removeFst [] y = error "list is empty!"
removeFst (x:xs) y | x == y = xs
				   | otherwise = x:removeFst xs y
				   
--ex1.13
count :: Char -> String -> Int
count c "" = 0
count c (x:xs) | c == x = 1 + count c xs
			   | otherwise = count c xs
				   
--ex1.14 init x performes as right recursion
blowup :: String -> String
blowup [] = []
blowup x = blowup (init x) ++ replicate (length x) (last x)

--ex1.15 simple sort
sortStr :: String -> String
sortStr [] = []
sortStr [x] = [x]
sortStr (x:y:xs) | x > y = sortStr(y:x:xs)
			   | otherwise = x:sortStr(y:xs)
			   
--ex1.17
isSubStr :: String -> String -> Bool
isSubStr [] ys = True
isSubStr xs [] = False
isSubStr (x:xs) (y:ys) = prefix (x:xs) (y:ys) 
							|| isSubStr (x:xs) ys
					 