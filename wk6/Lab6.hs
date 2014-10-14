module Lab6

where
import Data.List
import System.Random
import Week6

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]


-- ex 3
composites :: [Integer]
composites = filter (\x -> not (isPrime x)) [2..]

-- ex 4
manyTestF :: IO()
manyTestF = mapM_ (\x -> testF x) [1..6]

testF :: Int -> IO ()
testF k = testF' k composites

-- prints the least composite number which is false positive
testF' :: Int -> [Integer] -> IO ()
testF' k [] = print False
testF' k (x:xn) = do
    p <- primeF k x
    if p 
    then print x
    else testF' k xn

-- ex 5
-- instead of composites use carmichael

testFcarmichael :: Int -> IO ()
testFcarmichael k = testF' k carmichael

-- ex 6

manyTestMR :: IO()
manyTestMR = mapM_ (\x -> testMR x) [1..6]

testMR :: Int -> IO ()
testMR k = testMRcarmichael k composites

testMRcarmichael :: Int -> [Integer] -> IO ()
testMRcarmichael k [] = print False
testMRcarmichael k (x:xn) = do
    p <- primeMR k x
    if p 
    then print x
    else testMRcarmichael k xn



-- ex 7
-- takes forever after m20
testPP = testPrimesprimes primes

testPrimesprimes :: [Integer] -> IO ()
testPrimesprimes x = testPrimes $ map (\x -> fromIntegral x) x

testPrimes :: [Int] -> IO ()
testPrimes s = mapM_ (\x -> isAlsoPrime x) s

isAlsoPrime :: Int -> IO ()
isAlsoPrime p = do
    x <- primeMR 1 ((2^p) - 1)
    if x
    then do
        putStrLn ("2^" ++ show p ++ "-1")
        print ((2^p) - 1)
    else return ()