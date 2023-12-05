import D1Lib
import qualified System.IO

inputFile :: String
inputFile = "input/d1.txt"

main = do
    contents <- System.IO.readFile inputFile
    let part1Out = part1 contents
        part2Out = part2 contents
    putStrLn $ "p1: " ++ show part1Out
    putStrLn $ "p2: " ++ show part2Out