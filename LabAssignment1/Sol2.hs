module Sol2
where

import TAMO

--ex2.12
check1a = lequiv (not True) (False)
check1b = lequiv (not False) (True)
check2 = lequiv (\p -> p ==> False) (\p -> not p)
check3a = lequiv (\p -> p || True) (\p -> True) 		--in second lambda, p is argument to match argument sizes for the lequiv function, can also be replaced with const
check3b = lequiv (\p -> p && False) (\p -> False)
check4a = lequiv (\p -> p || False) (\p -> p)
check4b = lequiv (\p -> p && True) (\p -> p) --can also be replaced with id
check5 = lequiv (\p -> p || not p) (\p -> True)
check6 = lequiv (\p -> p && not p) (\p -> False)

--ex2.15 the 'not' keyword is used to return a True when every proposition returns False
checkCntr1 :: (Bool -> Bool) -> Bool
checkCntr1 bf = not (bf True) && not (bf False)

checkCntr2 :: (Bool -> Bool -> Bool) -> Bool
checkCntr2 bf = and [not(bf p q) | p <- [True, False], q <- [True, False]]

checkCntr3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
checkCntr3 bf = and [not(bf p q r) | p <- [True, False], 
										q <- [True, False], 
										r <- [True, False]]
-- 2.20
-- lequiv (\p q -> (not p) ==> q) (\p q -> p ==> (not q))  False
-- lequiv (\p q -> (not p) ==> q) (\p q -> q ==> (not p))  False
-- lequiv (\p q -> (not p) ==> q) (\p q -> (not q) ==> p)  True
-- lequiv (\p q r -> p ==> (q ==> r)) (\p q r -> q ==> (p ==> r)) True
-- lequiv (\p q r -> p ==> (q ==> r)) (\p q r -> (p ==> q) ==> r) False
-- lequiv (\p q -> (p ==> q) ==> p) (\p q -> p) True
-- lequiv (\p q r -> (p || q) ==> r) (\p q r -> (p ==> r) && (q ==> r)) True

--ex2.51
unique :: (a -> Bool) -> [a] -> Bool
unique p (x:xs) = length (filter p xs) == 1

--ex2.52
parity :: [Bool] -> Bool
parity xs = even (length (filter (\p -> p == True) xs))

--ex2.53
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p xs = parity (map p xs) -- or evenNR p = parity . map p