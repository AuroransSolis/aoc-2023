import qualified D1Lib
import qualified System.IO
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests"
    [ aocTestCase "d1p1" "input/d1test1.txt" D1Lib.part1 142
    , aocTestCase "d1p2 (1)" "input/d1test2.txt" D1Lib.part2 281
    , aocTestCase "d1p2 (2)" "input/d1test3.txt" D1Lib.part2 316
    ]

aocTestCase :: (Eq a, Show a) => String -> String -> (String -> a) -> a -> TestTree
aocTestCase name file fn result = testCase name $ testPart file fn result

testPart :: (Eq a, Show a) => String -> (String -> a) -> a -> Assertion
testPart file fn result = do
    fileContents <- System.IO.readFile file
    fn fileContents @?= result
