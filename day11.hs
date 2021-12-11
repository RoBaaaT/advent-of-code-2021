import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Util ( checkExamples )
import Data.List (sort)
import Data.Array
    ( Ix(inRange),
      Array,
      elems,
      (!),
      (//),
      bounds,
      indices,
      listArray )
import Data.Char (digitToInt)
import Data.Array.Base (amap)
import Data.List.Split (chunksOf)

main :: IO ()
main = do
    -- check example inputs
    let example_input = "5483143223\n\
\2745854711\n\
\5264556173\n\
\6141336146\n\
\6357385478\n\
\4167524645\n\
\2176841721\n\
\6882881134\n\
\4846848554\n\
\5283751526"
    checkExamples part1 1656 part2 195 example_input

    -- process the real inputs
    handle <- openFile "day11.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)
    hClose handle

part1 :: String -> Int
part1 rawInput = totalFlashes
    where
        input = parseInput rawInput
        (_, totalFlashes) = foldl (\(grid, flashes) _ -> let (newGrid, newFlashes, _) = simulateStep grid in (newGrid, flashes + newFlashes)) (input, 0) [1..100]

part2 :: String -> Int
part2 rawInput = length (takeWhile (not . snd) simulationSteps)
    where
        input = parseInput rawInput
        simulationSteps = scanl (\(grid, flashes) _ -> let (newGrid, _, allFlashed) = simulateStep grid in (newGrid, allFlashed)) (input, False) [1..]

-- returns the updated grid and the number of flashes that occurred
simulateStep :: OctoGrid -> (OctoGrid, Int, Bool)
simulateStep grid = (finalGrid, flashes, allFlashed)
    where
        updatedGrid = foldl updateIndex grid (indices grid)
        allFlashed = all ((== True) . snd) (elems updatedGrid)
        (finalGrid, flashes) = foldl (\(g, f) idx -> if snd (g ! idx) then (g // [(idx, (0, False))], f + 1) else (g, f)) (updatedGrid, 0) (indices grid)

updateIndex :: OctoGrid -> (Int, Int) -> OctoGrid
updateIndex grid idx
    | not (inRange (bounds grid) idx) = grid
    | currentEnergy >= 9 && not hasFlashed = foldl updateIndex (grid // [(idx, (currentEnergy + 1, True))]) [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x + 1, y), (x + 1, y + 1), (x, y + 1), (x - 1, y + 1), (x - 1, y)]
    | otherwise = grid // [(idx, (currentEnergy + 1, hasFlashed))]
    where
        (currentEnergy, hasFlashed) = grid ! idx
        (x, y) = idx

type OctoGrid = Array (Int, Int) (Int, Bool)

parseInput :: String -> OctoGrid
parseInput input = listArray ((0, 0), (height - 1, width - 1)) (zip (concat ls) (repeat False))
    where
        ls = map (map digitToInt) (lines input) :: [[Int]]
        width = length (head ls)
        height = length ls