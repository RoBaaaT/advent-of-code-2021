import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Data.Either (rights, fromRight)
import GHC.IO.Exception (assertError)
import Util (checkExamples)
import Data.Bits (shiftL)
import Data.List.Split (splitOn, splitOneOf)
import Data.Array ( Array, listArray, elems, range, bounds, (!) )
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    -- check example inputs
    let example_input = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
\\n\
\22 13 17 11  0\n\
\ 8  2 23  4 24\n\
\21  9 14 16  7\n\
\ 6 10  3 18  5\n\
\ 1 12 20 15 19\n\
\\n\
\ 3 15  0  2 22\n\
\ 9 18 13 17  5\n\
\19  8  7 25 23\n\
\20 11 10 24  4\n\
\14 21 16 12  6\n\
\\n\
\14 21 17 24  4\n\
\10 16 15  9 19\n\
\18  8 23 26 20\n\
\22 11 13  6  5\n\
\ 2  0 12  3  7"
    checkExamples part1 4512 part2 1924 example_input

    -- process the real inputs
    handle <- openFile "day4.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)
    hClose handle

part1 :: String -> Int
part1 rawInput = getWinningScore numbers boards
    where
        (numbers, boards) = parseInput rawInput

getWinningScore :: [Int] -> [Board] -> Int
getWinningScore [] _ = error "No board won"
getWinningScore (n:ns) boards = if null winningScores then getWinningScore ns updatedBoards else head winningScores
    where
        updatedBoards = map (markBoard n) boards
        winningScores = mapMaybe (detectWinAndScore n) updatedBoards

markBoard :: Int -> Board -> Board
markBoard number board = Board (listArray ((0, 0), (4, 4)) (map (\(num, marked) -> if num == number then (num, True) else (num, marked)) (elems (numbers board))))

detectWinAndScore :: Int -> Board -> Maybe Int
detectWinAndScore number board = if detectWin board then Just (score number board) else Nothing

score :: Int -> Board -> Int
score number board = number * sum (map fst (filter (\(n, m) -> not m) boardEntries))
    where
        boardEntries = elems (numbers board)

detectWin :: Board -> Bool
detectWin board = any (\x -> all ((== True) . (\i -> snd (boardEntries ! i))) (range ((x, ly), (x, hy)))) [lx..hx] || any (\y -> all ((== True) . (\i -> snd (boardEntries ! i))) (range ((lx, y), (hx, y)))) [ly..hy]
    where
        boardEntries = numbers board
        ((lx, ly), (hx, hy)) = bounds boardEntries

part2 :: String -> Int
part2 rawInput = getLastWinningScore numbers boards
    where
        (numbers, boards) = parseInput rawInput

getLastWinningScore :: [Int] -> [Board] -> Int
getLastWinningScore [] _ = error "Not all boards won"
getLastWinningScore (n:ns) [lastBoard] = case winningScore of
                                            Just s -> s
                                            Nothing -> getLastWinningScore ns [updatedBoard]
    where
        updatedBoard = markBoard n lastBoard
        winningScore = detectWinAndScore n updatedBoard
getLastWinningScore (n:ns) boards = getLastWinningScore ns unwonBoards
    where
        updatedBoards = map (markBoard n) boards
        unwonBoards = filter (not . detectWin) updatedBoards

newtype Board = Board { numbers :: Array (Int, Int) (Int, Bool) }
    deriving Show

parseInput :: String -> ([Int], [Board])
parseInput input = (map read (splitOn "," firstPart), map parseBoard remainingParts)
    where (firstPart:remainingParts) = filter (/= "") (splitOn "\n\n" input)

parseBoard :: String -> Board
parseBoard board = Board (listArray ((0, 0), (4, 4)) (zip numbers (repeat False)))
    where
        numberStrs = filter (/= "") (splitOneOf "\n " board)
        numbers = map read numberStrs :: [Int]