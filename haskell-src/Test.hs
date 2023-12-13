import qualified D1Lib
import qualified D2Lib
import qualified D3Lib
import qualified D4Lib
import Data.Functor ((<&>))
import qualified System.IO
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Unit tests"
        $ [ aocTestCase "d1p1" "input/d1test1.txt" D1Lib.part1 142
          , aocTestCase "d1p2 (1)" "input/d1test2.txt" D1Lib.part2 281
          , aocTestCase "d1p2 (2)" "input/d1test3.txt" D1Lib.part2 316
          , testCase "d2 readID" $ D2Lib.readID "123: a" 0 @?= ("a", 123)
          , aocTestCase "d2p1" "input/d2test.txt" D2Lib.part1 8
          , aocTestCase "d2p2" "input/d2test.txt" D2Lib.part2 2286
          , aocTestCase "d3p1" "input/d3test1.txt" D3Lib.part1 4361
          , aocTestCase "d3p2 (1)" "input/d3test1.txt" D3Lib.part2 467835
          , aocTestCase "d3p2 (2)" "input/d3test2.txt" D3Lib.part2 (14 * 13)
          , aocTestCase "d3p2 (3)" "input/d3test3.txt" D3Lib.part2 (14 * 13)
          , aocTestCase "d3p2 (4)" "input/d3test4.txt" D3Lib.part2 (14 * 13)
          , aocTestCase "d4p1" "input/d4test.txt" D4Lib.part1 13
          , aocTestCase "d4p2" "input/d4test.txt" D4Lib.part2 30
          ]
            ++ d3ExtraTests
  where
    d3ExtraTests = d3SingleTests ++ d3DoubleTests
    d3SingleTests =
        zipWith
            (\n input -> testCase ("d3p2 single (" ++ show n ++ ")") $ D3Lib.part2 input @?= 0)
            [1 .. length d3SingleTestCases]
            d3SingleTestCases
    d3SingleTestCases = ([makeD3SingleTestCase0, makeD3SingleTestCase1] <*> [1 .. 5]) ++ d3SingleTestCases1
    makeLine n = replicate n '.' ++ "111" ++ replicate (6 - n) '.'
    makeD3SingleTestCase0 n = ".........\n" ++ makeLine n ++ "\n....*....\n.........\n........."
    makeD3SingleTestCase1 n = ".........\n.........\n....*....\n" ++ makeLine n ++ "\n........."
    d3SingleTestCases1 =
        [ ".........\n.........\n.111*....\n.........\n........."
        , ".........\n.........\n....*111.\n.........\n........."
        ]
    d3DoubleTests =
        zipWith
            (\n input -> testCase ("d3p2 double (" ++ show n ++ ")") $ D3Lib.part2 input @?= 111 * 111)
            [1 .. length d3DoubleTestCases]
            d3DoubleTestCases
    d3DoubleTestCases =
        concatMap d3MakeDoubleTestCase1 [1 .. 5]
            ++ [d3MakeDoubleTestCase2 a b | a <- [1 .. 5], b <- [1 .. 5]]
            ++ concatMap d3MakeDoubleTestCase3 [1 .. 5]
            ++ [".........\n.........\n.111*111.\n.........\n........."]
            ++ d3DoubleTestCases4
    d3MakeDoubleTestCase1 a =
        [ ".........\n" ++ topLine ++ "\n.111*....\n.........\n........."
        , ".........\n" ++ topLine ++ "\n....*111.\n.........\n........."
        ]
      where
        topLine = makeLine a
    d3MakeDoubleTestCase2 a b = ".........\n" ++ makeLine a ++ "\n....*....\n" ++ makeLine b ++ "\n........."
    d3MakeDoubleTestCase3 b =
        [ ".........\n.........\n.111*....\n" ++ botLine ++ "\n........."
        , ".........\n.........\n....*111.\n" ++ botLine ++ "\n........."
        ]
      where
        botLine = makeLine b
    d3DoubleTestCases4 =
        [ ".........\n.111.111.\n....*....\n.........\n........."
        , ".........\n.........\n.111*111.\n.........\n........."
        , ".........\n.........\n....*....\n.111.111.\n........."
        ]

aocTestCase :: (Eq a, Show a) => String -> String -> (String -> a) -> a -> TestTree
aocTestCase name file fn result = testCase name $ testPart file fn result

testPart :: (Eq a, Show a) => String -> (String -> a) -> a -> Assertion
testPart file fn result = do
    fileContents <- System.IO.readFile file
    fn fileContents @?= result
