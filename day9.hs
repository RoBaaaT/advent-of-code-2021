import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Util ( checkExamples )
import Data.Array ( Array, listArray, indices, bounds, (!) )
import Data.List (sort, elemIndex)
import Data.Char (digitToInt)

main :: IO ()
main = do
    -- check example inputs
    let example_input = "2199943210\n\
\3987894921\n\
\9856789892\n\
\8767896789\n\
\9899965678"
    let heightmap = parseInput example_input
    print (findBasinLocations heightmap [(0,1)] [(0,0), (0,2), (1,1)])
    print (findBasinLocations heightmap [(0,9)] [(1,9), (0,8)])
    print (reverse (sort (map (basinSize heightmap) (findLowPoints heightmap))))
    checkExamples part1 15 part2 1134 example_input

    -- process the real inputs
    handle <- openFile "day9.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)
    hClose handle

part1 :: String -> Int
part1 rawInput = sum (map (\idx -> (input ! idx) + 1) (findLowPoints input))
    where
        input = parseInput rawInput

part2 :: String -> Int
part2 rawInput = product largestThreeSizes
    where
        input = parseInput rawInput
        largestThreeSizes = take 3 (reverse (sort (map (basinSize input) (findLowPoints input))))

findLowPoints :: Array (Int, Int) Int -> [(Int, Int)]
findLowPoints heightmap = filter (isLowPoint heightmap) (indices heightmap)

isLowPoint :: Array (Int, Int) Int -> (Int, Int) -> Bool
isLowPoint heightmap (y, x) = height < northHeight && height < southHeight && height < westHeight && height < eastHeight
    where
        ((minY, minX), (maxY, maxX)) = bounds heightmap
        height = heightmap ! (y, x)
        northHeight = if y /= minY then heightmap ! (y - 1, x) else maxBound :: Int
        southHeight = if y /= maxY then heightmap ! (y + 1, x) else maxBound :: Int
        westHeight = if x /= minX then heightmap ! (y, x - 1) else maxBound :: Int
        eastHeight = if x /= maxX then heightmap ! (y, x + 1) else maxBound :: Int

basinSize :: Array (Int, Int) Int -> (Int, Int) -> Int
basinSize heightmap loc = length basinLocations
    where
        (y, x) = loc
        (basinLocations, _) = findBasinLocations heightmap [loc] (filterBounds (bounds heightmap) [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)])

findBasinLocations :: Array (Int, Int) Int -> [(Int, Int)] -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
findBasinLocations heightmap basinLocations [] = (basinLocations, [])
findBasinLocations heightmap basinLocations (loc:locs)
    | (heightmap ! loc) == 9 = findBasinLocations heightmap basinLocations locs
    | otherwise = findBasinLocations heightmap (loc : basinLocations) (locs ++ newLocs)
    where
        (y, x) = loc
        newLocs = filter (\newLoc -> notElem newLoc basinLocations && notElem newLoc locs) (filterBounds (bounds heightmap) [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)])

filterBounds :: ((Int, Int), (Int, Int)) -> [(Int, Int)] -> [(Int, Int)]
filterBounds ((minY, minX), (maxY, maxX)) = filter (\(y, x) -> y >= minY && x >= minX && y <= maxY && x <= maxX)

parseInput :: String -> Array (Int, Int) Int
parseInput input = listArray ((0, 0), (height - 1, width - 1)) (concat ls)
    where
        ls = map (map digitToInt) (lines input) :: [[Int]]
        width = length (head ls)
        height = length ls