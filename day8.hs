import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Util
import Data.List.Split (splitOn)
import Data.List (sort, elemIndex)

main :: IO ()
main = do
    -- check example inputs
    let exampleInput = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n\
\edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n\
\fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n\
\fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n\
\aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n\
\fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n\
\dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n\
\bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n\
\egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n\
\gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
    checkExamples part1 26 part2 61229 exampleInput

    -- process the real inputs
    handle <- openFile "day8.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)
    hClose handle

part1 :: String -> Int
part1 rawInput = sum (map (\(_, outputs) -> length (filter (\o -> length o == 2 || length o == 3 || length o == 4 || length o == 7) outputs)) input)
    where
        input = parseInput rawInput

part2 :: String -> Int
part2 rawInput = sum (map solveDisplay input)
    where
        input = parseInput rawInput

solveDisplay :: ([String], [String]) -> Int
solveDisplay (uniques, outputs) = 1000 * thousands + 100 * hundreds + 10 * tens + ones
    where
        sortedUniques = map sort uniques
        sortedOutputs = map sort outputs
        patterns = solvePatterns sortedUniques
        ones = mapDigit (sortedOutputs !! 3) patterns
        tens = mapDigit (sortedOutputs !! 2) patterns
        hundreds = mapDigit (sortedOutputs !! 1) patterns
        thousands = mapDigit (head sortedOutputs) patterns

solvePatterns :: [String] -> [String]
solvePatterns sortedUniques = [zeroPattern, onePattern, twoPattern, threePattern, fourPattern, fivePattern, sixPattern, sevenPattern, eightPattern, ninePattern]
    where
        onePattern = head (filter ((==) 2 . length) sortedUniques)
        fourPattern = head (filter ((==) 4 . length) sortedUniques)
        sevenPattern = head (filter ((==) 3 . length) sortedUniques)
        eightPattern = "abcdefg"
        ninePattern = head (filter (\p -> length p == 6 && all (`elem` p) fourPattern) sortedUniques)
        threePattern = head (filter (\p -> length p == 5 && all (`elem` p) onePattern) sortedUniques)
        e = head (filter (`notElem` ninePattern) eightPattern)
        zeroPattern = head (filter (\p -> length p == 6 && all (`elem` p) sevenPattern && p /= ninePattern) sortedUniques)
        twoPattern = head (filter (\p -> length p == 5 && elem e p) sortedUniques)
        fivePattern = head (filter (\p -> length p == 5 && all (`elem` ninePattern) p && p /= threePattern) sortedUniques)
        sixPattern = head (filter (\p -> length p == 6 && all (`elem` p) fivePattern && p /= ninePattern) sortedUniques)

mapDigit :: String -> [String] -> Int
mapDigit digit patterns = case index of
                            Just i -> i
                            _ -> error "Pattern not found"
    where
        index = elemIndex digit patterns

parseInput :: String -> [([String], [String])]
parseInput input = map parseInputLine (lines input)

parseInputLine :: String -> ([String], [String])
parseInputLine line = (words uniques, words outputs)
    where
        [uniques, outputs] = splitOn " | " line