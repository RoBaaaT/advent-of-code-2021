import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Util ( checkExamples )
import Data.List.Split (splitOn)

main :: IO ()
main = do
    -- check example inputs
    let exampleInput = "target area: x=20..30, y=-10..-5"
    checkExamples part1 45 part2 112 exampleInput

    -- process the real inputs
    handle <- openFile "day17.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)
    hClose handle

part1 :: String -> Int
part1 rawInput = maximumY $ last $ filter valid $ map (checkVerticalTrajectory (minY, maxY)) [-200..200]
    where
        ((_, _), (minY, maxY)) = parseInput rawInput

part2 :: String -> Int
part2 rawInput = sum validHorizontalCounts
    where
        ((minX, maxX), (minY, maxY)) = parseInput rawInput
        validVerticals = filter valid $ map (checkVerticalTrajectory (minY, maxY)) [-200..200]
        validHorizontalCounts = map (validHorizontalCount (minX, maxX) . validSteps) validVerticals

valid :: (Bool, Int, [Int]) -> Bool
valid (result, _, _) = result
maximumY :: (Bool, Int, [Int]) -> Int
maximumY (_, result, _) = result
validSteps :: (Bool, Int, [Int]) -> [Int]
validSteps (_, _, result) = result

checkVerticalTrajectory :: (Int, Int) -> Int -> (Bool, Int, [Int])
checkVerticalTrajectory (minY, maxY) velY = (not (null validSteps), max, validSteps)
    where
        trajectory = takeWhile (\(y, _) -> y >= minY) $ scanl (\(y, velY) _ -> (y + velY, velY - 1)) (0, velY) [0..]
        max = maximum $ map fst trajectory
        validSteps = map fst $ filter (\(_, y) -> y >= minY && y <= maxY) $ zip [0..] $ map fst trajectory

validHorizontalCount :: (Int, Int) -> [Int] -> Int
validHorizontalCount (minX, maxX) validSteps = length $ filter (== True) $ map (checkHorizontalTrajectory (minX, maxX) validSteps) [1..maxX]

checkHorizontalTrajectory :: (Int, Int) -> [Int] -> Int -> Bool
checkHorizontalTrajectory (minX, maxX) validSteps velX = any (\finalX -> finalX >= minX && finalX <= maxX) finalXs
    where
        finalXs = map (\steps -> fst $ foldl (\(x, velX) _ -> (x + velX, if velX > 0 then velX - 1 else if velX < 0 then velX + 1 else 0)) (0, velX) [1..steps]) validSteps

-- input parsing
parseInput :: String -> ((Int, Int), (Int, Int))
parseInput input = ((minX, maxX), (minY, maxY))
    where
        [xpart, ypart] = map ((\[coord, range] -> range) . splitOn "=") (splitOn ", " input)
        [minX, maxX] = map read $ splitOn ".." xpart
        [minY, maxY] = map read $ splitOn ".." ypart
