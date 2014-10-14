module Lab6ex2


where
import Criterion.Main
import Week6

-- criterion fails to install.

main = defaultMain [
    bgroup "exM" [ bench "2 3 5"  $ whnf exM 2 3 5
               , bench "2 16 6"  $ whnf exM 2 16 6
               , bench "2 1024 5"  $ whnf exM 2 1024 5
               , bench "31 4095 5" $ whnf exM 31 4095 5
               ]
    ]
