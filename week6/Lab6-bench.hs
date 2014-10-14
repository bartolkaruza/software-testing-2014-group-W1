module Main where

    import Lab6
    import Week6
    import Criterion.Main

    base1 = 1
    exponent1 = 2
    modValue1 = 3
    base2 = 10
    exponent2 = 3000
    modValue2 = 20
    base3 = 3
    exponent3 = 67108864
    modValue3 = 2048


    main :: IO()
    main = defaultMain [
            bgroup "small numbers" [ 
                  bench "expM 1,2,3" $ whnf (expM base1 exponent1) modValue1
                , bench "exM 1,2,3" $ whnf (exM base1 exponent1) modValue1
                ],
            bgroup "large exponent" [ 
                  bench "expM 10,3000,20" $ whnf (expM base2 exponent2) modValue2
                , bench "exM 10,3000,20" $ whnf (exM base2 exponent2) modValue2
                ],
            bgroup "huge exponent" [ 
                  bench "expM 3,67108864,2048" $ whnf (expM base3 exponent3) modValue3
                , bench "exM 3,67108864,2048" $ whnf (exM base3 exponent3) modValue3
                ]
            ]

    testEx :: (Integer -> Integer -> Integer -> Integer) -> (Integer -> Integer -> Integer -> Integer) -> IO()
    testEx f1 f2 = do
        print $ ("f1 1 2 30: " ++ show (f1 base1 exponent1 modValue1))
        print $ ("f2 1 2 30: " ++ show (f2 base1 exponent1 modValue1))
        print "--------------------------"
        print $ ("f1 10 3000 20: " ++ show (f1 base2 exponent2 modValue2))
        print $ ("f2 10 3000 20: " ++ show (f2 base2 exponent2 modValue2))
        print "--------------------------"
        print $ ("f1 323 67108864 1024: " ++ show (f1 base3 exponent3 modValue3))
        print $ ("f2 323 67108864 1024: " ++ show (f2 base3 exponent3 modValue3))
