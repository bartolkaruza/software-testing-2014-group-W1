module Main where

    import Lab4
    import Test.QuickCheck
    import Test.Hspec
    import SetOrd
    import System.Random
    import Data.List

    main :: IO()
    main = 
        do 
        x <- getRandomSet 1000
        print x

--------------------------------
-- Ex.1 (Time spent: ) TODO
-- Ex.2 (Time spent: 1 hr)
-- Ex.3 (Time spent: 2 hr)

    quickCheckTest' :: IO()
    quickCheckTest' = verboseCheck ((\s l-> (list2set s) == (list2set l)) :: [Int] -> [Int] -> Bool)

	
    getRandomSet :: Int -> IO (Set Int)
    getRandomSet n = do
                    l <- getRandomInts n
                    return (list2set l)

    getRandomInts :: Int -> IO [Int]
    getRandomInts n = do
                    g <- getStdGen
                    return (take n (randoms g :: [Int]))

--------------------------------
-- Ex.4 (1.5 hr.)

    testIntersect1 :: IO()
    testIntersect1 = quickCheck ((\s l -> subSet (intersectSet (list2set s) (list2set l) ) (list2set s) ) :: [Int] -> [Int] -> Bool)

    testIntersect2 :: IO()
    testIntersect2 = quickCheck ((\s l -> subSet (intersectSet (list2set s) (list2set l) ) (list2set l) ) :: [Int] -> [Int] -> Bool)

    intersectSet :: (Ord a) => Set a -> Set a -> Set a
    intersectSet (Set []) _ = Set []
    intersectSet _ (Set []) = Set []
    intersectSet (Set xs) (Set ys) = unionSet (Set(filter (\x -> elem x ys) xs)) (Set(filter (\y -> elem y xs) ys))

    testDifference1 :: IO()
    testDifference1 = quickCheck ((\s l -> let x = (difference (list2set s) (list2set l)) in not (subSet x (Set s) ) || x == (Set [])) :: [Int] -> [Int] -> Bool)

    testDifference2 :: IO()
    testDifference2 = quickCheck ((\s l -> let x = (difference (list2set s) (list2set l)) in not (subSet x (Set l) ) || x == (Set [])) :: [Int] -> [Int] -> Bool)

    difference :: (Ord a) => Set a -> Set a -> Set a
    difference set1 set2 = deleteWholeSet set2 (deleteWholeSet set1 set2)

    deleteWholeSet :: Ord a => Set a -> Set a -> Set a
    deleteWholeSet (Set []) set2 = set2
    deleteWholeSet (Set (x:xs)) set2 = deleteWholeSet (Set(xs)) (deleteSet x set2)

-- Ex.5

    type Rel a = [(a,a)]

    infixr 5 @@

    (@@) :: Eq a => Rel a -> Rel a -> Rel a
    r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w]

    trClos :: Ord a => Rel a -> Rel a
    trClos [] = []
    trClos (x:xs) = x : trClos (trClos ([x]@@xs) ++ xs)

    trClosSingle :: Ord a => Rel a -> Rel a -> Rel a
    trClosSingle x [] = []
    trClosSingle x (y:ys) = x ++ trClosSingle x (x @@ [y])

