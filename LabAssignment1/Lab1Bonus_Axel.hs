module Lab1Bonus_Axel
where

import Lab1Bonus

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
foldrPlus xs ys = foldr (\x y -> x:y) ys xs 		--last ys is identity element, that is appended in the last fold (as the last x:y)

foldrReverse :: [a] -> [a]
foldrReverse xs = foldr (\x y -> y ++ [x]) [] xs

foldlReverse :: [a] -> [a]
foldlReverse xs = foldl (\x y -> y:x) [] xs

--foldr, not foldl
--foldl will build the entire expression by iterating through the whole list (and then evaluate everything backwards). Because the last item of the list is evaluated first (the outermost item of the expression foldl has built. If the list is infinite, this will eventually cause a stack overflow
--foldr applies the given function to the next value in each iteration and the result of the following folds. Because it runs forward it doesn't have to go through the entire list first to create results and therefore may work on infinite lists.

--bonus ex4
sign3, sign4 :: (Creature, Creature) -> Bool
sign3 (x,y) = x == Lady || y == Lady
sign4 (x,y) = x == Tiger && (y == Tiger || y == Lady)

solution2 :: [(Creature,Creature)]
solution2 = [ (x,y) | x <- [Lady,Tiger],
					  y <- [Lady,Tiger],
					  sign3(x,y) == sign4(x,y) ]			  
--this leads to [(Tiger,Lady)]