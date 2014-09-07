module Lab1Bonus
where

foldrLen :: [a] -> Int
foldrLen xs = foldr (\x y -> y + 1) 0 xs

--Eq(a) indicates that 'a' is of a type that can be compared for equality
foldrElem :: Eq(a) => a -> [a] -> Bool
foldrElem e xs = foldr (\x y -> (e == x) || y) False xs

foldrOr :: [Bool] -> Bool
foldrOr xs = foldr (||) False xs -- (||) is shorthand for (\x y -> x || y)

foldrMap :: (a -> b) -> [a] -> [b]
foldrMap f xs = foldr (\x y -> f x : y) [] xs

foldrFilter :: (a -> Bool) -> [a] -> [a]
foldrFilter f xs = foldr (\x y -> if f x then x : y else y) [] xs

foldrPlus :: [a] -> [a] -> [a]
foldrPlus xs ys = foldr (\x y -> x:y) ys xs 		--last ys is identity element, that is appended in the last fold (as the last x:y)(

foldrReverse :: [a] -> [a]
foldrReverse xs = foldr (\x y -> y ++ [x]) [] xs