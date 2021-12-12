import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Util ( checkExamples )
import Data.Array (Array, listArray, Ix (range), (!))
import Data.List.Split (splitOn)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Char (isLower)

main :: IO ()
main = do
    -- check example inputs
    let exampleInput1 = "start-A\n\
\start-b\n\
\A-c\n\
\A-b\n\
\b-d\n\
\A-end\n\
\b-end"
    checkExamples part1 10 part2 36 exampleInput1
    let exampleInput2 = "dc-end\n\
\HN-start\n\
\start-kj\n\
\dc-start\n\
\dc-HN\n\
\LN-dc\n\
\HN-end\n\
\kj-sa\n\
\kj-HN\n\
\kj-dc"
    checkExamples part1 19 part2 103 exampleInput2

    -- process the real inputs
    handle <- openFile "day12.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)
    hClose handle

part1 :: String -> Int
part1 rawInput = length (findAllPaths input (start input) [])
    where
        input = parseInput rawInput

part2 :: String -> Int
part2 rawInput = length (findAllPathsAlt input (start input) False [])
    where
        input = parseInput rawInput

findAllPaths :: Graph -> Int -> [Int] -> [[Int]]
findAllPaths graph current visited
    | current == end graph = [visited ++ [current]]
    | isSmall graph current && current `elem` visited = []
    | otherwise = concatMap (\next -> findAllPaths graph next (visited ++ [current])) (neighbors graph current)

findAllPathsAlt :: Graph -> Int -> Bool -> [Int] -> [[Int]]
findAllPathsAlt graph current smallVisitedTwice visited
    | current == end graph = [visited ++ [current]]
    | isSmall graph current && elem current visited && (smallVisitedTwice || (current == start graph)) = []
    | otherwise = concatMap (\next -> findAllPathsAlt graph next smallVisitedTwiceNew (visited ++ [current])) (neighbors graph current)
    where
        smallVisitedTwiceNew = smallVisitedTwice || (isSmall graph current && elem current visited)

data Graph = Graph { vertices :: [String], adj :: Array (Int, Int) Bool, start, end :: Int }
    deriving Show

isSmall :: Graph -> Int -> Bool
isSmall graph caveId = isLower (head cave)
    where
        cave = vertices graph !! caveId

neighbors :: Graph -> Int -> [Int]
neighbors graph caveId = filter (isAdjacent graph caveId) [0..size graph - 1]
    where
        adjs = adj graph

isAdjacent :: Graph -> Int -> Int -> Bool
isAdjacent graph a b = (!) (adj graph) (a, b)

size :: Graph -> Int
size graph = length $ vertices graph

parseInput :: String -> Graph
parseInput input = Graph vertices adj (fromJust (elemIndex "start" vertices)) (fromJust (elemIndex "end" vertices))
    where
        paths = map (\line -> let [from, to] = splitOn "-" line in (from, to)) (lines input)
        allVertices = map fst paths ++ map snd paths
        vertices = foldl (\verts vert -> if vert `elem` verts then verts else verts ++ [vert]) [] allVertices
        pathsByIndex = map (\(f, t) -> (fromJust (elemIndex f vertices), fromJust (elemIndex t vertices))) paths
        vertexCount = length vertices
        adjBounds = ((0, 0), (vertexCount - 1, vertexCount - 1))
        adj = listArray adjBounds (map (\(f, t) -> (f, t) `elem` pathsByIndex || (t, f) `elem` pathsByIndex) (range adjBounds))