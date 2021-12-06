import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Util
import Data.List.Split (splitOn, splitOneOf)
import qualified Data.HashMap.Strict as HM

main :: IO ()
main = do
    -- check example inputs
    let example_input = "0,9 -> 5,9\n\
\8,0 -> 0,8\n\
\9,4 -> 3,4\n\
\2,2 -> 2,1\n\
\7,0 -> 7,4\n\
\6,4 -> 2,0\n\
\0,9 -> 2,9\n\
\3,4 -> 1,4\n\
\0,0 -> 8,8\n\
\5,5 -> 8,2"
    checkExamples part1 5 part2 12 example_input

    -- process the real inputs
    handle <- openFile "day5.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)
    hClose handle

newtype CloudMap = CloudMap { opacities :: HM.HashMap (Int, Int) Int }
    deriving Show

part1 :: String -> Int
part1 rawInput = length (filter (>= 2) (HM.elems (opacities cloudMap)))
    where
        lines = parseInput rawInput
        emptyMap = CloudMap HM.empty
        cloudMap = foldl markLine emptyMap lines

part2 :: String -> Int
part2 rawInput = length (filter (>= 2) (HM.elems (opacities cloudMap)))
    where
        lines = parseInput rawInput
        emptyMap = CloudMap HM.empty
        cloudMap = foldl markLineIncludingDiagonals emptyMap lines

markLine :: CloudMap -> Line -> CloudMap
markLine cloudMap line
    | x (from line) == x (to line) && y (from line) <= y (to line) = foldl markPoint cloudMap (Prelude.map (Point (x (from line))) [y (from line)..y (to line)])
    | x (from line) == x (to line) = foldl markPoint cloudMap (map (Point (x (from line))) [y (to line)..y (from line)])
    | y (from line) == y (to line) && x (from line) <= x (to line) = foldl markPoint cloudMap (map (\x -> Point x (y (from line))) [x (from line)..x (to line)])
    | y (from line) == y (to line) = foldl markPoint cloudMap (map (\x -> Point x (y (from line))) [x (to line)..x (from line)])
    | otherwise = cloudMap

markLineIncludingDiagonals :: CloudMap -> Line -> CloudMap
markLineIncludingDiagonals cloudMap line
    | x (from line) == x (to line) && y (from line) <= y (to line) = foldl markPoint cloudMap (Prelude.map (Point (x (from line))) [y (from line)..y (to line)])
    | x (from line) == x (to line) = foldl markPoint cloudMap (map (Point (x (from line))) [y (to line)..y (from line)])
    | y (from line) == y (to line) && x (from line) <= x (to line) = foldl markPoint cloudMap (map (\x -> Point x (y (from line))) [x (from line)..x (to line)])
    | y (from line) == y (to line) = foldl markPoint cloudMap (map (\x -> Point x (y (from line))) [x (to line)..x (from line)])
    | otherwise = foldl markPoint cloudMap (pointsOnDiagonal line)

pointsOnDiagonal :: Line -> [Point]
pointsOnDiagonal line
    | x (to line) >= x (from line) && y (to line) >= y (from line) = map (\t -> Point (originX + t) (originY + t)) [0..(x (to line) - x (from line))]
    | x (to line) >= x (from line) && y (to line) < y (from line) = map (\t -> Point (originX + t) (originY - t)) [0..(x (to line) - x (from line))]
    | x (to line) < x (from line) && y (to line) >= y (from line) = map (\t -> Point (originX - t) (originY + t)) [0..(x (from line) - x (to line))]
    | x (to line) < x (from line) && y (to line) < y (from line) = map (\t -> Point (originX - t) (originY - t)) [0..(x (from line) - x (to line))]
    | otherwise = []
    where
        originX = x (from line)
        originY = y (from line)

markPoint :: CloudMap -> Point -> CloudMap
markPoint cloudMap point = CloudMap (HM.insert (x point, y point) newValue opcs)
    where
        opcs = opacities cloudMap
        newValue = 1 + HM.lookupDefault 0 (x point, y point) opcs

parseInput :: String -> [Line]
parseInput input = map parseLine ls
    where
        ls = lines input