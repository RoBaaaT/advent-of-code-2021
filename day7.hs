import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Util
import Data.List.Split (splitOn)

main :: IO ()
main = do
    -- check example inputs
    let example_input = "16,1,2,0,4,2,7,1,2,14"
    checkExamples part1 37 part2 168 example_input

    -- process the real inputs
    handle <- openFile "day7.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)
    hClose handle

part1 :: String -> Int
part1 rawInput = minimum fuelCostsPerPosition
    where
        crabs = parseInput rawInput
        fuelCostsPerPosition = map (calculateTotalFuelCosts crabs) [0..maximum crabs]

calculateTotalFuelCosts :: [Int] -> Int -> Int
calculateTotalFuelCosts crabs position = sum (map (\crab -> abs (crab - position)) crabs)

part2 :: String -> Int
part2 rawInput = minimum fuelCostsPerPosition
    where
        crabs = parseInput rawInput
        fuelCostsPerPosition = map (calculateRealTotalFuelCosts crabs) [0..maximum crabs]

calculateRealTotalFuelCosts :: [Int] -> Int -> Int
calculateRealTotalFuelCosts crabs position = sum (map (\crab -> fuelCostByDistance (abs (crab - position))) crabs)

fuelCostByDistance :: Int -> Int
fuelCostByDistance dist = div (dist * (dist + 1)) 2

parseInput :: String -> [Int]
parseInput input = map read crabs
    where
        crabs = filter (/= "\n") (splitOn "," input)