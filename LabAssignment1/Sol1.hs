module Sol1
where

import GS

--ex1.1
ex1a = 3*7
ex1b = 3-7
ex1c = 7/3
ex1d = 7^3

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
				   
--ex1.14 init x performes as right recursion (or we could just pass an integer along with the blowup call)
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

--ex1.18
-- 	:t ["test1", "test2"]
--	["test1", "test2"] :: [[Char]]
-- 	:t (True, "Test")
--	(True, "Test") :: (Bool, [Char])
--	:t [(True, "Test"), (False, "Test")]
--	[(True, "Test"), (False, "Test")] :: [(Bool, [Char])]
--	:t ([True, False], "Test")
-- 	([True, False], "Test") :: ([Bool], [Char])
--	:t not 					//the inverse operator
--  not :: Bool -> Bool

--ex1.19
--  *Sol1> :t head				//returns first element of list
--  head :: [a] -> a
--  *Sol1> :t last				//returns last element of list						
--  last :: [a] -> a
--  *Sol1> :t init				//returns all elements of list except last
--  init :: [a] -> [a]
--  *Sol1> :t fst				
--  fst :: (a, b) -> a			//returns first element of tuple
--  *Sol1> :t (++)
--  (++) :: [a] -> [a] -> [a]	//takes 2 lists and returns them combined as 1 list
--  *Sol1> :t flip				//takes function in a and processes the arguments flipped. So flip (/) 2 1 returns 0.5 instead of 2
--  flip :: (a -> b -> c) -> b -> a -> c	
--  *Sol1> :t flip (++)			//when concatenating 2 lists, elements of the second argument will appended first
--  flip (++) :: [a] -> [a] -> [a]

--ex1.20
lengths :: [[a]] -> [Int]
lengths xs = map length xs

--ex1.21
sumLengths :: [[a]] -> Int
sumLengths xs = sum (lengths xs)

--ex1.24
-- it still works, because the first incorrect ldp call (without argument) automatically triggers
-- a new correct ldp call through the 3d case of the 'prime' function

