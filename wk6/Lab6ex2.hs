module Main


where
--import Criterion.Main
import Week6

-- criterion fails to install.


main = do
    print $ exM 31 4095 5
    print $ exM 2 4095 5

{-
main = defaultMain [
    bgroup "exM" [ bench "2 3 5"  $ whnf exM 2 3 5
               , bench "2 16 6"  $ whnf exM 2 16 6
               , bench "2 1024 5"  $ whnf exM 2 1024 5
               , bench "31 4095 5" $ whnf exM 31 4095 5
               ]
    ]

-}