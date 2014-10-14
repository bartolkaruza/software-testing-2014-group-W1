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

