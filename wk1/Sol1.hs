module Sol1 where

	import GS

	-- 1.9
	maxInt :: [Int] -> Int
	maxInt [] = error "empty list"
	maxInt [x] = x
	maxInt (x:xs) = max x (maxInt xs)

	-- 1.10
	removeFst :: [Int] -> Int -> [Int]
	removeFst [] y = []
	removeFst (x:xs) y = 
		if x == y
			then xs
			else x : removeFst xs y

	-- 1.13
	count :: Char -> String -> Int
	count x [] = 0
	count x (y:ys) = 
		if x == y
			then 1 + count x ys
			else count x ys

	-- 1.14 (right recursion)
	blowup :: String -> String
	blowup [] = []
	blowup x = blowup (init x) ++ replicate (length x) (last x)

	-- 1.15 (quicksort)
	srtString :: [String] -> [String]
	srtString [] = []
	srtString (x:xs) = (srtString lesser) ++ [x] ++ (srtString greater)
		where
			lesser  = filter (< x) xs
			greater = filter (>= x) xs

	-- prefix
	prefix :: String -> String -> Bool
	prefix [] ys = True
	prefix (x:xs) [] = False
	prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

	-- 1.17
	substring :: String -> String -> Bool
	substring (x:xs) (y:ys) | null xs || null ys = False
							| prefix (x:xs) (y:ys) = True
							| substring (x:xs) ys = True
							| otherwise = False

	-- 1.20
	lengths :: [[a]] -> [Int]
	lengths x = map length x

	-- 1.21
	sumLengths :: [[a]] -> Int
	sumLengths x = sum (lengths x)

