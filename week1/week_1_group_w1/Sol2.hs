module Sol2 where

    import GS
    import TAMO

    -- 2.13
    c1   = logEquiv1 (\p -> False) (\p -> not True)
    c2   = logEquiv1 (\p -> p ==> False) (\p -> not p)
    c3_1 = logEquiv1 (\p -> p || True) (\p -> True)
    c3_2 = logEquiv1 (\p -> p && False) (\p -> False)
    c4_1 = logEquiv1 (\p -> p || False) id
    c4_2 = logEquiv1 (\p -> p && True) id
    c5   = logEquiv1 (\p -> p || not p) (\p -> True)
    c6   = logEquiv1 (\p -> p && not p) (\p -> False)

    -- 2.15
    checkCntr1 :: (Bool -> Bool) -> Bool
    checkCntr1 bf = not (bf True) && not (bf False)

    checkCntr2 :: (Bool -> Bool -> Bool) -> Bool
    checkCntr2 bf = and [not(bf p q) | p <- [True, False], q <- [True, False]]

    checkCntr3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
    checkCntr3 bf = and [not(bf p q r) | p <- [True, False], 
                                         q <- [True, False], 
                                         r <- [True, False]]

    -- 2.20
    p1 = logEquiv2 (\p q -> not p ==> q) (\p q -> p ==> not q)
    p2 = logEquiv2 (\p q -> not p ==> q) (\p q -> q ==> not p)
    p3 = logEquiv2 (\p q -> not p ==> q) (\p q -> not q ==> p)
    p4 = logEquiv3 (\p q r -> p ==> (q ==> r)) (\p q r -> q ==> (p ==> r))
    p5 = logEquiv3 (\p q r -> p ==> (q ==> r)) (\p q r -> (p ==> q) ==> r)
    p6 = logEquiv2 (\p q -> (p ==> q) ==> p) (\p q -> p)
    p7 = logEquiv3 (\p q r -> (p || q) ==> r) (\p q r -> (p ==> r) || (q ==> r))

    -- 2.51
    unique :: (a -> Bool) -> [a] -> Bool
    unique p xs = length (filter p xs) == 1

    -- 2.52
    parity :: [Bool] -> Bool
    parity xs = even (length (filter (\x -> x == True) xs))

    -- 2.53
    evenNR :: (a -> Bool) -> [a] -> Bool
    evenNR p xs = parity (map p xs)
    -- alternative:
    evenNRa p = parity . map p
    