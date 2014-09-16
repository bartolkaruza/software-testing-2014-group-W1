module Main where

	import System.Random
	import Data.List

	main :: IO ()
	main = do
		putStrLn("Enter the participants line by line")
		putStrLn("Finish by entering an empty line")
		xs <- getLines
		x <- randomRIO(0, length (deran xs)) :: IO Int
		printResults (zip xs ((deran xs)!!x))

	getLines :: IO [String]
	getLines = do
		x <- getLine
		if x == "" then return [] else do
			xs <- getLines
			return (x:xs)

	perms :: Eq a => [a] -> [[a]]
	perms [] = [[]]
	perms xs = [x:ys | x <- xs, ys <- perms(delete x xs)]

	isDerangement :: [String] -> [String] -> Bool
	isDerangement [] [] = True
	isDerangement _ [] = False
	isDerangement [] _ = False
	isDerangement (x:xs) (y:ys) = not (stringEq x y) && (isDerangement xs ys)

	deran :: [String] -> [[String]]
	deran xs = filter (\x -> isDerangement x xs) (perms xs)	

	stringEq :: [Char] -> [Char] -> Bool
	stringEq [] [] = True
	stringEq (x:xs) (y:ys) = x == y && stringEq xs ys
	stringEq _ _ = False

	printResults :: [(String, String)] -> IO()
	printResults [] = return ()
	printResults ((x,y):xs) = do 
		putStrLn (x ++ " buys a gift for " ++ y)
		printResults xs