import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Util ( checkExamples )
import Data.Array (Array, listArray, (!), bounds, (//), Ix (inRange, range), indices, assocs, array, elems)
import Data.Char (digitToInt)
import Data.List.Split (chunksOf)

main :: IO ()
main = do
    -- check example inputs
    let exampleInput = "1163751742\n\
\1381373672\n\
\2136511328\n\
\3694931569\n\
\7463417111\n\
\1319128137\n\
\1359912421\n\
\3125421639\n\
\1293138521\n\
\2311944581"
    checkExamples part1 40 part2 315 exampleInput

    -- process the real inputs
    handle <- openFile "day15.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)
    hClose handle

part1 :: String -> Int
part1 rawInput = (!) accumulatedRiskMap (snd (bounds accumulatedRiskMap)) - (!) riskMap (0, 0)
    where
        riskMap = parseInput rawInput
        accumulatedRiskMap = accumulateRisks riskMap

part2 :: String -> Int
part2 rawInput = (!) accumulatedRiskMap (snd (bounds accumulatedRiskMap)) - (!) riskMap (0, 0)
    where
        riskMap = parseInput rawInput
        enlargedMap = enlarge5x riskMap
        accumulatedRiskMap = accumulateRisks enlargedMap

accumulateRisks :: RiskMap -> RiskMap
accumulateRisks riskMap = foldl accumulateRiskForRow riskMap [0..size]
    where
        size = snd $ snd $ bounds riskMap

accumulateRiskForRow :: RiskMap -> Int -> RiskMap
accumulateRiskForRow riskMap y = (//) riskMap (zip (range ((0, y), (size, y))) (tail (scanl (\left x -> if y == 0 then (if x == 0 then 0 else left) + (!) riskMap (x, y) else min ((!) riskMap (x, y - 1)) left + (!) riskMap (x, y)) 9999 [0..size])))
    where
        size = snd $ snd $ bounds riskMap

enlarge5x :: RiskMap -> RiskMap
enlarge5x riskMap = enlargedMap
    where
        originalSize = snd (snd $ bounds riskMap) + 1
        newSize = originalSize * 5 - 1
        enlargedMap = array ((0, 0), (newSize, newSize)) (concatMap (\(mulX, mulY) -> map (\((x, y), risk) -> ((x + mulX * originalSize, y + mulY * originalSize), increaseRisk (mulX, mulY) risk)) (assocs riskMap)) (range ((0, 0), (4, 4))))

increaseRisk :: (Int, Int) -> Int -> Int
increaseRisk (mulX, mulY) risk = wrapRisk newRisk
    where
        newRisk = risk + mulX + mulY

wrapRisk :: Int -> Int
wrapRisk risk
    | risk > 9 = wrapRisk (risk - 9)
    | otherwise = risk

-- input parsing
type RiskMap = Array (Int, Int) Int

parseInput :: String -> RiskMap
parseInput input = listArray ((0, 0), (height - 1, height - 1)) (concat ls)
    where
        ls = map (map digitToInt) (lines input) :: [[Int]]
        height = length ls
