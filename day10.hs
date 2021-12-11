import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Util ( checkExamples )
import Data.List (sort)

main :: IO ()
main = do
    -- check example inputs
    let exampleInput = "[({(<(())[]>[[{[]{<()<>>\n\
\[(()[<>])]({[<{<<[]>>(\n\
\{([(<{}[<>[]}>{[]{[(<()>\n\
\(((({<>}<{<{<>}{[]{[]{}\n\
\[[<[([]))<([[{}[[()]]]\n\
\[{[{({}]{}}([{[{{{}}([]\n\
\{<[[]]>}<{[{[{[]{()[[[]\n\
\[<(<(<(<{}))><([]([]()\n\
\<{([([[(<>()){}]>(<<{{\n\
\<{([{{}}[<[[[<>{}]]]>[]]"
    checkExamples part1 26397 part2 288957 exampleInput

    -- process the real inputs
    handle <- openFile "day10.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)
    hClose handle

part1 :: String -> Int
part1 rawInput = sum (filter (> 0) (map scoreLine input))
    where
        input = lines rawInput

part2 :: String -> Int
part2 rawInput = -(scores !! div (length scores) 2)
    where
        input = lines rawInput
        scores = sort (filter (< 0) (map scoreLine input))

scoreLine :: String -> Int
scoreLine line = scoreLineInternal line []

scoreLineInternal :: String -> [Char] -> Int
scoreLineInternal "" stack = -(foldl (\b a -> b * 5 + missingScore (closingCharacter a)) 0 stack)
scoreLineInternal (c:cs) []
    | isOpeningCharacter c = scoreLineInternal cs [c]
    | otherwise = illegalScore c
scoreLineInternal (c:cs) (o:os)
    | isOpeningCharacter c = scoreLineInternal cs (c:o:os)
    | c == closingCharacter o = scoreLineInternal cs os
    | otherwise = illegalScore c

isOpeningCharacter :: Char -> Bool
isOpeningCharacter c
    | c == '(' = True
    | c == '[' = True
    | c == '{' = True
    | c == '<' = True
    | otherwise = False

closingCharacter :: Char -> Char
closingCharacter c
    | c == '(' = ')'
    | c == '[' = ']'
    | c == '{' = '}'
    | c == '<' = '>'
    | otherwise = error "not an opening character"

illegalScore :: Char -> Int
illegalScore c
    | c == ')' = 3
    | c == ']' = 57
    | c == '}' = 1197
    | c == '>' = 25137
    | otherwise = error "not a closing character"

missingScore :: Char -> Int
missingScore c
    | c == ')' = 1
    | c == ']' = 2
    | c == '}' = 3
    | c == '>' = 4
    | otherwise = error "not a closing character"