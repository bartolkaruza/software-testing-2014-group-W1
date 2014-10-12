module Main where

    import Lab6
    import Week6
    import Criterion.Main

    main :: IO()
    main = defaultMain [
        bgroup "small numbers" [ 
              bench "expM 1,2,3" $ whnf (expM 1 2) 3
            , bench "exM' 1,2,3" $ whnf (exM' 1 2) 3
            ],
        bgroup "large exponent" [ 
              bench "expM 10,3000,20" $ whnf (expM 10 3000) 20
            , bench "exM' 10,3000,20" $ whnf (exM' 10 3000) 20
            ],
        bgroup "huge exponent" [ 
              bench "expM 10,10000,20" $ whnf (expM 10 10000) 20
            , bench "exM' 10,10000,20" $ whnf (exM' 10 10000) 20
            ]
        ]