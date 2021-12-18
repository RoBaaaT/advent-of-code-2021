import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Util ( checkExamples )
import Data.List.Split (splitOn)
import Data.Char (digitToInt)

main :: IO ()
main = do
    -- check example inputs
    let exampleInput = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n\
\[[[5,[2,8]],4],[5,[[9,9],0]]]\n\
\[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n\
\[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n\
\[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n\
\[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n\
\[[[[5,4],[7,7]],8],[[8,3],8]]\n\
\[[9,3],[[9,9],[6,[4,9]]]]\n\
\[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n\
\[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
    checkExamples part1 4140 part2 3993 exampleInput

    -- process the real inputs
    handle <- openFile "day18.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)
    hClose handle

part1 :: String -> Int
part1 rawInput = magnitude $ foldl1 add numbers
    where
        numbers = parseInput rawInput

part2 :: String -> Int
part2 rawInput = maximum $ map magnitude sums
    where
        numbers = parseInput rawInput
        pairs = [ (x, y) | x <- numbers, y <- numbers ]
        sums = map (\(a, b) -> add a b) pairs ++ map (\(a, b) -> add b a) pairs

add :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
add a b = reduce $ Pair a b

reduce :: SnailfishNumber -> SnailfishNumber
reduce a = case explode a of
    Just (a', _, _) -> reduce a'
    Nothing -> case split a of
        Just a' -> reduce a'
        Nothing -> a

explode :: SnailfishNumber -> Maybe (SnailfishNumber, Int, Int)
explode = explode' 0

explode' :: Int -> SnailfishNumber -> Maybe (SnailfishNumber, Int, Int)
explode' depth (Pair l r) =
    if depth == 4
    then Just (Number 0, fromnum l, fromnum r)
    else
        case explode' (depth + 1) l of
            Just (l', ln, rn) -> Just (Pair l' (addleftmost rn r), ln, 0)
            Nothing ->
                case explode' (depth + 1) r of
                    Just (r', ln, rn) -> Just (Pair (addrightmost ln l) r', 0, rn)
                    Nothing -> Nothing

explode' _ n = Nothing

fromnum :: SnailfishNumber -> Int
fromnum (Number n) = n
fromnum p = error $ show p ++ " is not a number"

addleftmost :: Int -> SnailfishNumber -> SnailfishNumber
addleftmost n (Pair l r) = Pair (addleftmost n l) r
addleftmost n (Number m) = Number $ n + m

addrightmost :: Int -> SnailfishNumber -> SnailfishNumber
addrightmost n (Pair l r) = Pair l $ addrightmost n r
addrightmost n (Number m) = Number $ n + m

split :: SnailfishNumber -> Maybe SnailfishNumber
split (Pair a b) = case split a of
                            Just a' -> Just $ Pair a' b
                            Nothing -> case split b of
                                Just b' -> Just $ Pair a b'
                                Nothing -> Nothing
split (Number a) = if a >= 10 then Just $ Pair (Number $ div a 2) (Number $ div (a + 1) 2) else Nothing

magnitude :: SnailfishNumber -> Int
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b
magnitude (Number n) = n

-- input parsing
data SnailfishNumber = Pair SnailfishNumber SnailfishNumber | Number Int
    deriving (Eq)
instance Show SnailfishNumber where
    show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"
    show (Number n) = show n

parseInput :: String -> [SnailfishNumber]
parseInput input = map (fst . parseSnailfishNumber) $ lines input

parseSnailfishNumber :: String -> (SnailfishNumber, String)
parseSnailfishNumber input
    | first == '[' = parseSnailfishNumberPair $ tail input
    | otherwise = (Number $ digitToInt first, tail input)
    where first = head input

parseSnailfishNumberPair :: String -> (SnailfishNumber, String)
parseSnailfishNumberPair input = (Pair left right, tail remainder)
    where
        (left, rstr) = parseSnailfishNumber input
        (right, remainder) = if head rstr == ',' then parseSnailfishNumber $ tail rstr else error "missing ',' in snailfish number pair"