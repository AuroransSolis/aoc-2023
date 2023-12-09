import qualified D1Lib
import qualified D2Lib
import qualified D3Lib
import qualified D4Lib
import qualified System.IO
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Unit tests"
        [ aocTestCase "d1p1" "input/d1test1.txt" D1Lib.part1 142
        , aocTestCase "d1p2 (1)" "input/d1test2.txt" D1Lib.part2 281
        , aocTestCase "d1p2 (2)" "input/d1test3.txt" D1Lib.part2 316
        , testCase "d2 readID" $ D2Lib.readID "123: a" 0 @?= ("a", 123)
        , aocTestCase "d2p1" "input/d2test.txt" D2Lib.part1 8
        , aocTestCase "d2p2" "input/d2test.txt" D2Lib.part2 2286
        , aocTestCase "d3p1" "input/d3test.txt" D3Lib.part1 4361
        , aocTestCase "d3p2" "input/d3test.txt" D3Lib.part2 467835
        , aocTestCase "d4p1" "input/d4test.txt" D4Lib.part1 13
        ]

aocTestCase :: (Eq a, Show a) => String -> String -> (String -> a) -> a -> TestTree
aocTestCase name file fn result = testCase name $ testPart file fn result

testPart :: (Eq a, Show a) => String -> (String -> a) -> a -> Assertion
testPart file fn result = do
    fileContents <- System.IO.readFile file
    fn fileContents @?= result
