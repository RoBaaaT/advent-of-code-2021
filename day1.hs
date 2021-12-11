import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Data.Either (rights)
import GHC.IO.Exception (assertError)
import Util (parseInt, checkExamples)

main :: IO ()
main = do
    -- check example inputs
    let exampleInputs = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
    checkExamples part1 7 part2 5 exampleInputs

    -- process the real inputs
    handle <- openFile "day1.txt" ReadMode
    input <- hGetContents handle
    let ls = lines input
    let inputs = rights (map parseInt ls)
    print (part1 inputs)
    print (part2 inputs)
    hClose handle

part1 :: [Int] -> Int
part1 [] = 0
part1 [_] = 0
part1 (d1:d2:ds) = (if d1 < d2 then 1 else 0) + part1 (d2:ds)

part2 :: [Int] -> Int
part2 [] = 0
part2 [_] = 0
part2 [_, _] = 0
part2 [_, _, _] = 0
part2 (d1:d2:d3:d4:ds) = (if d1 < d4 then 1 else 0) + part2 (d2:d3:d4:ds)
