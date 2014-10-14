module Main


where
--import Criterion.Main
import Week6

-- criterion fails to install.


main = do
    testM 10000000

testToIO a =
    return ()

someTest = do
    testToIO $ (exM 2 16 6)
    testToIO $ (exM 2 1024 5)
    testToIO $ (exM 31 4095 5)
    testToIO $ (exM 128 4095 5)
    testToIO $ (exM 2047 4095 5)


testM i = if i == 0 
         then print "Done"
         else do
                someTest
                testM (i-1)


{-
main = defaultMain [
    bgroup "exM" [ bench "2 3 5"  $ whnf exM 2 3 5
               , bench "2 16 6"  $ whnf exM 2 16 6
               , bench "2 1024 5"  $ whnf exM 2 1024 5
               , bench "31 4095 5" $ whnf exM 31 4095 5
               ]
    ]

-}