import qualified D1Lib
import qualified System.IO
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests"
    [ testCase "d1p1" $ testPart "input/d1test1.txt" D1Lib.part1 142
    , testCase "d1p2 (1)" $ testPart "input/d1test2.txt" D1Lib.part2 281
    , testCase "d1p2 (2)" $ testPart "input/d1test3.txt" D1Lib.part2 316
    ]

testPart :: String -> (String -> Int) -> Int -> Assertion
testPart file fn result = do
    fileContents <- System.IO.readFile file
    fn fileContents @?= result
