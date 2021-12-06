module Util(parseInt, IntParseError, checkExamples, Point (..), Line (..), parsePoint, parseLine) where
import Data.List.Split (splitOn)

newtype IntParseError = IntParseError String

parseInt :: String -> Either IntParseError Int
parseInt "" = Left (IntParseError "Empty string")
parseInt (c:cs) = case parseIntFromChar c of
    Left err -> Left err
    Right i1 -> case cs of
        "" -> Right i1
        _ -> case parseInt cs of
                Left err -> Left err
                Right i2 -> Right (i1 * 10 ^ length cs + i2)

parseIntFromChar :: Char -> Either IntParseError Int
parseIntFromChar c = case c of
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

checkExamples :: Eq o1 => Eq o2 => (input -> o1) -> o1 -> (input -> o2) -> o2 -> input -> IO ()
checkExamples part1 expected1 part2 expected2 inputs = do
    if part1 inputs == expected1 then
        if part2 inputs == expected2 then
            putStrLn "test with example inputs passed"
        else
            putStrLn "test with example inputs failed for part 2"
    else
        putStrLn "test with example inputs failed for part 1"

data Point = Point { x, y :: Int }
    deriving Show

parsePoint :: String -> Point
parsePoint str = Point (read x) (read y)
    where [x,y] = splitOn "," str

data Line = Line { from, to :: Point }
    deriving Show

parseLine :: String -> Line
parseLine str = Line from to
    where
        [left,right] = splitOn " -> " str
        from = parsePoint left
        to = parsePoint right
