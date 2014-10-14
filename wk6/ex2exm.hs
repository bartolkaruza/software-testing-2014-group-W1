module Main

where
import Week6

-- performance test for ex. 2 by running linux 'time'
-- used in stead of Criterion

-- exM test

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