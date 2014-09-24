module Main where

    import Lab4
    import Test.QuickCheck
    import Test.Hspec
    import SetOrd
    import System.Random

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
-- Ex.4

    testIntersect :: IO()
    testIntersect = verboseCheck ((\s l -> subSet (intersectSet (Set s) (Set l) ) (Set s) ) :: [Int] -> [Int] -> Bool)

    intersectSet :: (Ord a) => Set a -> Set a -> Set a
    intersectSet (Set []) _ = Set []
    intersectSet _ (Set []) = Set []
    intersectSet (Set xs) (Set ys) = unionSet (Set(filter (\x -> elem x ys) xs)) (Set(filter (\y -> elem y xs) ys))

    difference :: (Ord a) => Set a -> Set a -> Set a
    difference set1 set2 = deleteWholeSet set1 set2
    -- testDeleteWholeSet :: Ord a => Set a -> Set a -> Set a
    -- testDeleteWholeSet = verboseCheck ((\s l -> not (subSet (Set(s)) (deleteWholeSet (Set(s)) (Set(l))) ) ) :: [Int] -> [Int] -> Bool)

    deleteWholeSet :: Ord a => Set a -> Set a -> Set a
    deleteWholeSet (Set []) set2 = set2
    deleteWholeSet (Set (x:xs)) set2 = deleteWholeSet (Set(xs)) (deleteSet x set2)

    -- unionSet :: (Ord a) => Set a -> Set a -> Set a 
    -- unionSet (Set [])     set2  =  set2
    -- unionSet (Set (x:xs)) set2  = insertSet x (unionSet (Set xs) set2)