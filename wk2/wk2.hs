module Lab2 where

    import Data.List
    import System.Random

    -- ex. 1 (time: 1 hour)

    data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq, Show)

    isRectangular :: Integer -> Integer -> Integer -> Bool
    isRectangular x y z | x^2 + y^2 == z^2 = True
                        | y^2 + z^2 == x^2 = True
                        | z^2 + x^2 == y^2 = True
                        | otherwise = False

    isIsosceles :: Integer -> Integer -> Integer -> Bool
    isIsosceles x y z | (x == y) || (x == z) || (y == z) = True
                      | otherwise = False

    triangle :: Integer -> Integer -> Integer -> Shape
    triangle x y z | (x == y) && (y == z) = Equilateral
                   | isRectangular x y z = Rectangular
                   | isIsosceles x y z = Isosceles
                   | x + y < z || y + z < x || z + x < y = NoTriangle
                   | otherwise = Other

    testRectangles = [ (x,y,z) | x <- [1..20], y <- [1..20], z <- [1..20], x^2 + y^2 == z^2 ]
    
    testIsosceles = [ (x,y,z) | x <- [1..10], y <- [1..10], z <- [1..10], 
                                       ((x == y) && not (y == z)) || 
                                       ((y == z) && not (z == x)) || 
                                       ((z == x) && not (x == y)) ]

    testEquilateral = [ (x,y,z) | x <- [1..10], y <- [1..10], z <- [1..10], (x == y) && (y == z) ]


    triangleTest :: [(Integer, Integer, Integer)] -> Shape -> Bool
    triangleTest [] s = True
    triangleTest ((a,b,c):xs) s = (triangle a b c == s) && triangleTest xs s

    --triangleTest testRectangles Rectangular
    --triangleTest testIsosceles Isosceles
    --triangleTest testEquilateral Equilateral

    -- ex. 2 (time: 1/2 hour)
    isPermutation :: Eq a => [a] -> [a] -> Bool
    isPermutation [] [] = True
    isPermutation [] _ = False -- lists of uneven length
    isPermutation _ [] = False -- lists of uneven length
    isPermutation (x:xs) ys = elem x ys && isPermutation xs (delete x ys)

    -- ex. 3 (time: 1/3 hour)

    reverseIsPermutation = isPermutation [x | x <- [1..100]] [x | x <- reverse [1..100] ] -- is reversable
    differentLengthIsPermutation = isPermutation [x | x <- [1..100]] [x | x <- reverse [1..99] ] -- has same length

    -- ex. 4 (time: 1/2 hour)

    perms :: Eq a => [a] -> [[a]]
    perms [] = [[]]
    perms xs = [(x:y) | x <- xs, y <- perms (delete x xs)]


    -- ex. 5 (time: 1/3 hour)

    isDerangement :: Eq a => [a] -> [a] -> Bool
    isDerangement a b = if isPermutation a b
                            then and (zipWith (/=) a b)
                            else False

    -- ex. 6 (time: 1/4 hour)
    deran :: Eq a => [a] -> [b]
    deran a = filter (isDerangement a) (perms a)

    -- ex. 7 (time 1/2 hour)
    reverseDeran = isDerangement [x | x <- [1..100]] [x | x <- reverse [1..100] ] -- is reversable


