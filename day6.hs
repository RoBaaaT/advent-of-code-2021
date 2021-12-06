import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Util
import Data.List.Split (splitOn)

main :: IO ()
main = do
    -- check example inputs
    let example_input = "3,4,3,1,2"
    checkExamples part1 5934 part2 26984457539 example_input

    -- process the real inputs
    handle <- openFile "day6.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)
    hClose handle

part1 :: String -> Int
--part1 rawInput = length (simulate fish 80)
--    where
--        fish = parseInput rawInput
part1 rawInput = sum (simulateCounts countsByTimer 80)
    where
        fish = parseInput rawInput
        countsByTimer = map (count fish) [0..8]

simulate :: [Int] -> Int -> [Int]
simulate fish days
    | days <= 0 = fish
    | otherwise = simulate (simulateDay fish) (days - 1)

simulateDay :: [Int] -> [Int]
simulateDay fish = map (\timer -> if timer > 0 then timer - 1 else 6) fish ++ map (const 8) (filter (== 0) fish)

part2 :: String -> Int
part2 rawInput = sum (simulateCounts countsByTimer 256)
    where
        fish = parseInput rawInput
        countsByTimer = map (count fish) [0..8]

count :: [Int] -> Int -> Int
count fish timer = length (filter (== timer) fish)

simulateCounts :: [Int] -> Int -> [Int]
simulateCounts counts days
    | days <= 0 = counts
    | otherwise = simulateCounts (simulateCountsDay counts) (days - 1)

simulateCountsDay :: [Int] -> [Int]
simulateCountsDay [zeroes, ones, twos, threes, fours, fives, sixes, sevens, eights] = [ones, twos, threes, fours, fives, sixes, zeroes + sevens, eights, zeroes]
simulateCountsDay _ = error "Invalid count list length!"

parseInput :: String -> [Int]
parseInput input = map read fish
    where
        fish = filter (/= "\n") (splitOn "," input)