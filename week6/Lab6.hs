module Lab6 where
    
    import Data.List
    import System.Random
    import Week6

    carmichael :: [Integer]
    carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
          k <- [2..], 
          isPrime (6*k+1), 
          isPrime (12*k+1), 
          isPrime (18*k+1) ]

    
    exM' :: Integer -> Integer -> Integer -> Integer
    exM' _ 0 _ = 1
    exM' x 1 z = mod x z
    exM' x y z = mod (x * (exM' x (y-1) z)) z

    exM'' :: Integer -> Integer -> Integer -> Integer
    exM'' x y z | n == z = rem (x^y) z
                | otherwise = (exMuP x n z) * (rem (x^(y-n)) z)
                    where n = exponentOf2 y

    exMuP :: Integer -> Integer -> Integer -> Integer
    exMuP x 2 z = (rem x z)^2
    exMuP x y z = (exMuP x (exponentOf2 (y-1)) z)^2

    exponentOf2 x = last $ takeWhile (\y -> x >= y) [2^x | x <- [1..]]
    -- exM'' :: Integer -> Integer -> Integer -> Integer
    -- exM'' x y z 
    --     | isEven y = 

    -- exMuP :: Integer -> Integer -> Integer -> Integer -> Integer
    -- exMUp x y z g | y == g = x
    --     | otherwise exMUp (rem (x^y)) z g

    isEven :: Integer -> Bool
    isEven x = rem x 2 == 0
    -- Tests random numbers against exM' and exM and compares performance
    -- ex_test_M' :: Integer -> IO Bool
    -- ex_test_M' n = do
    --     a <- randomRIO (1, n 02) :: IO Integer


    -- Ex 4 
    -- Unoptimized (sieve-less) composites
    composites :: [Integer]
    composites = filter (\x -> not (isPrime x)) [2..]

    
-- Ex 5
-- testF


-- testExM