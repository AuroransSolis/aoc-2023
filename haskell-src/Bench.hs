import Criterion.Main
import qualified D1Lib
import qualified D2Lib
import qualified D3Lib
import qualified D4Lib
import qualified D5Lib
import qualified D6Lib
import qualified D7Lib
import qualified System.IO

type BenchData = (String, String -> Int, String -> Int)

benchData :: [BenchData]
benchData =
    [ ("d1", D1Lib.part1, D1Lib.part2)
    , ("d2", D2Lib.part1, D2Lib.part2)
    , ("d3", D3Lib.part1, D3Lib.part2)
    , ("d4", D4Lib.part1, D4Lib.part2)
    -- , ("d5", D5Lib.part1, D5Lib.part2)
    , ("d6", D6Lib.part1, D6Lib.part2)
    , ("d7", D7Lib.part1, D7Lib.part2)
    ]

buildBench :: BenchData -> Benchmark
buildBench (day, p1, p2) = env setupEnv setupGroup
  where
    setupGroup contents = bgroup day [bench "p1" $ whnf p1 contents, bench "p2" $ whnf p2 contents]
    setupEnv = readFile ("input/" ++ day ++ ".txt")

main = defaultMain $ map buildBench benchData
