import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Data.Either (rights)
import GHC.IO.Exception (assertError)

main :: IO ()
main = do
    -- check example inputs
    let example_inputs = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
    if part1 example_inputs == 7 && part2 example_inputs == 5
    then
        putStrLn "test with example inputs passed"
    else
        putStrLn "test with example inputs failed"

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

newtype IntParseError = IntParseError String

parseInt :: String -> Either IntParseError Int
parseInt "" = Left (IntParseError "Empty string")
parseInt (c:cs) = case parseChar c of
    Left err -> Left err
    Right i1 -> case cs of
        "" -> Right i1
        _ -> case parseInt cs of
                Left err -> Left err
                Right i2 -> Right (i1 * 10 ^ length cs + i2)

parseChar :: Char -> Either IntParseError Int
parseChar c = case c of
    '0' -> Right 0
    '1' -> Right 1
    '2' -> Right 2
    '3' -> Right 3
    '4' -> Right 4
    '5' -> Right 5
    '6' -> Right 6
    '7' -> Right 7
    '8' -> Right 8
    '9' -> Right 9
    _ -> Left (IntParseError ("Unexpected character: " ++ [c]))
