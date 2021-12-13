import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Util ( checkExamples )
import Data.Array (Array, listArray, Ix (range, inRange), (!), (//), elems, array, bounds, assocs)
import Data.List.Split (splitOn, chunksOf)
import Data.List (elemIndex, transpose)
import Data.Maybe (fromJust)
import Data.Char (isLower)

main :: IO ()
main = do
    -- check example inputs
    let exampleInput = "6,10\n\
\0,14\n\
\9,10\n\
\0,3\n\
\10,4\n\
\4,11\n\
\6,0\n\
\6,12\n\
\4,1\n\
\0,13\n\
\10,12\n\
\3,4\n\
\3,0\n\
\8,4\n\
\1,10\n\
\2,14\n\
\8,10\n\
\9,0\n\
\\n\
\fold along y=7\n\
\fold along x=5"
    checkExamples part1 17 part2 "#####\n#...#\n#...#\n#...#\n#####\n.....\n.....\n" exampleInput

    -- process the real inputs
    handle <- openFile "day13.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    putStrLn (part2 input)
    hClose handle

part1 :: String -> Int
part1 rawInput = length (filter (== True) (elems foldedPaper))
    where
        (paper, instructions) = parseInput rawInput
        foldedPaper = fold paper (head instructions)

part2 :: String -> String
part2 rawInput = unlines (map (\y -> map (\x -> if (!) foldedPaper (x, y) then '#' else '.') [0..maxX]) [0..maxY])
    where
        (paper, instructions) = parseInput rawInput
        foldedPaper = foldl fold paper instructions
        ((_, _), (maxX, maxY)) = bounds foldedPaper

fold :: Paper -> FoldInstruction -> Paper
fold paper (Horizontal val) = foldUp paper val
fold paper (Vertical val) = foldLeft paper val

foldUp :: Paper -> Int -> Paper
foldUp paper val = array ((0, 0), (maxX, val - 1)) (map (\((x, y), e) -> ((x, y), e || mirrorLookupY paper (x, y) val)) (filter (\((x, y), e) -> y < val) (assocs paper)))
    where
        ((_, _), (maxX, maxY)) = bounds paper

foldLeft :: Paper -> Int -> Paper
foldLeft paper val = array ((0, 0), (val - 1, maxY)) (map (\((x, y), e) -> ((x, y), e || mirrorLookupX paper (x, y) val)) (filter (\((x, y), e) -> x < val) (assocs paper)))
    where
        ((_, _), (maxX, maxY)) = bounds paper

mirrorLookupX :: Paper -> (Int, Int) -> Int -> Bool
mirrorLookupX paper (x, y) val
    | inRange (bounds paper) pos = (!) paper pos
    | otherwise = False
    where
        pos = (val + (val - x), y)

mirrorLookupY :: Paper -> (Int, Int) -> Int -> Bool
mirrorLookupY paper (x, y) val
    | inRange (bounds paper) pos = (!) paper pos
    | otherwise = False
    where
        pos = (x, val + (val - y))

type Paper = Array (Int, Int) Bool
data FoldInstruction = Vertical Int | Horizontal Int deriving (Eq, Ord, Read, Show)

parseInput :: String -> (Paper, [FoldInstruction])
parseInput input = (paper, foldInstructions)
    where
        [p, fis] = splitOn "\n\n" input
        dots = map parseDot (lines p)
        maxX = maximum (map fst dots)
        maxY = maximum (map snd dots)
        emptyPaper = listArray ((0, 0), (maxX, maxY)) (repeat False)
        paper = (//) emptyPaper (zip dots (repeat True))
        foldInstructions = map parseFold (lines fis)

parseDot :: String -> (Int, Int)
parseDot line = (read xRaw, read yRaw)
    where
        [xRaw, yRaw] = splitOn "," line

parseFold :: String -> FoldInstruction
parseFold line
    | ins == "fold along x" = Vertical (read val)
    | ins == "fold along y" = Horizontal (read val)
    | otherwise = error "Invalid fold instruction"
    where
        [ins, val] = splitOn "=" line