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

    -- Simple recursive exM' which does not use double exponentiation
    -- but does improve performance
    exM' :: Integer -> Integer -> Integer -> Integer
    exM' _ 0 _ = 1
    exM' x 1 z = mod x z
    exM' x y z = mod (x * (exM' x (y-1) z)) z

    -- exM'' :: Integer -> Integer -> Integer -> Integer
    -- exM'' x y z 
    --     | isEven y = 

    -- exMuP :: Integer -> Integer -> Integer -> Integer -> Integer
    -- exMUp x y z g | y == g =  

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