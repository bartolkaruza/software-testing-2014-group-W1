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
    removeFst (x:xs) y | x == y = xs
                       | otherwise = x:removeFst xs y

    -- 1.13
    count :: Char -> String -> Int
    count x [] = 0
    count x (y:ys) | x == y = 1 + count x ys
                   | otherwise = count x ys

    -- 1.14
    blowup :: String -> String
    blowup [] = []
    blowup x = blowup (init x) ++ replicate (length x) (last x)

    -- 1.15 (simple sort)
    srtStringS :: [String] -> [String]
    srtStringS [] = []
    srtStringS [x] = [x]
    srtStringS (x:y:xs) | x > y = srtStringS(y:x:xs)
                        | otherwise = x : srtStringS(y:xs)

    -- 1.15 alternative, quicksort (faster)
    srtStringQ :: [String] -> [String]
    srtStringQ [] = []
    srtStringQ (x:xs) = (srtStringQ lesser) ++ [x] ++ (srtStringQ greater)
        where
            lesser  = filter (< x) xs
            greater = filter (>= x) xs

    -- 1.15 routing
    srtString = srtStringQ

    -- prefix
    prefix :: String -> String -> Bool
    prefix [] ys = True
    prefix (x:xs) [] = False
    prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

    -- 1.17
    substring :: String -> String -> Bool
    substring [] ys = True
    substring xs [] = False
    substring (x:xs) (y:ys) = prefix (x:xs) (y:ys) || substring (x:xs) ys

    -- 1.20
    lengths :: [[a]] -> [Int]
    lengths x = map length x

    -- 1.21
    sumLengths :: [[a]] -> Int
    sumLengths x = sum (lengths x)
