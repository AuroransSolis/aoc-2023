import Criterion.Main
import qualified D1Lib
import qualified D2Lib
import qualified System.IO

type BenchData = (String, String -> Int, String -> Int)

benchData :: [BenchData]
benchData =
    [ ("d1", D1Lib.part1, D1Lib.part2)
    , ("d2", D2Lib.part1, D2Lib.part2)
    ]

buildBench :: BenchData -> Benchmark
buildBench (day, p1, p2) = env setupEnv setupGroup
  where
    setupGroup contents = bgroup day [bench "p1" $ whnf p1 contents, bench "p2" $ whnf p2 contents]
    setupEnv = readFile ("input/" ++ day ++ ".txt")

main = defaultMain $ map buildBench benchData
