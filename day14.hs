import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Util ( checkExamples )
import Data.List.Split (splitOn)
import Data.Map (fromListWith, elems, Map, assocs)
import Data.Map.Lazy (toList)
import Data.List (sort)

main :: IO ()
main = do
    -- check example inputs
    let exampleInput = "NNCB\n\
\\n\
\CH -> B\n\
\HH -> N\n\
\CB -> H\n\
\NH -> C\n\
\HB -> C\n\
\HC -> B\n\
\HN -> C\n\
\NN -> C\n\
\BH -> H\n\
\NC -> B\n\
\NB -> B\n\
\BN -> B\n\
\BB -> N\n\
\BC -> B\n\
\CC -> N\n\
\CN -> C"
    checkExamples part1 1588 part2 2188189693529 exampleInput

    -- process the real inputs
    handle <- openFile "day14.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)
    hClose handle

part1 :: String -> Int
part1 rawInput = head sortedFreqs - last sortedFreqs
    where
        (template, rules) = parseInput rawInput
        polymer = foldl (\temp i -> growPolymer rules temp) template [1..10] -- grow 10x
        charFreqs = fromListWith (+) [(c, 1) | c <- polymer]
        sortedFreqs = reverse (sort (elems charFreqs))

part2 :: String -> Int
part2 rawInput = head sortedFreqs - last sortedFreqs
    where
        (template, rules) = parseInput rawInput
        pairs = zip template (tail template)
        pairFreqs = fromListWith (+) [(p, 1) | p <- pairs]
        freqs = foldl (\pfs i -> growFrequencies rules pfs) pairFreqs [1..40] -- grow 40x
        charFreqs = fromListWith (+) (map (\((l, r), f) -> (r, f)) (assocs freqs))
        sortedFreqs = reverse (sort (elems charFreqs))

-- naive approach (used in part 1)
growPolymer :: [Rule] -> String -> String
growPolymer rules template = result ++ [last template]
    where
        pairs = zip template (tail template)
        result = concatMap (\(l, r) -> l : getMatch rules l r) pairs

getMatch :: [Rule] -> Char -> Char -> String
getMatch rules l r
    | length matchingRules == 1 = [insertion (head matchingRules)]
    | null matchingRules = ""
    | otherwise = error "more than one rule matched"
    where
        matchingRules = filter (\rule -> l == left rule && r == right rule) rules

-- frequency-based approach (used in part 2)
growFrequencies :: [Rule] -> Map (Char, Char) Int -> Map (Char, Char) Int
growFrequencies rules freqs = fromListWith (+) newFrequencies
    where
        newFrequencies = concatMap (applyRule rules) (assocs freqs)

applyRule :: [Rule] -> ((Char, Char), Int) -> [((Char, Char), Int)]
applyRule rules ((l, r), f) = [((l, i), f), ((i, r), f)]
    where
        i = getSingleMatch rules l r

getSingleMatch :: [Rule] -> Char -> Char -> Char
getSingleMatch rules l r
    | length matchingRules == 1 = insertion (head matchingRules)
    | null matchingRules = error "no rule matched"
    | otherwise = error "more than one rule matched"
    where
        matchingRules = filter (\rule -> l == left rule && r == right rule) rules

-- input parsing
data Rule = Rule { left, right, insertion :: Char }
    deriving Show

parseInput :: String -> (String, [Rule])
parseInput input = (template, rules)
    where
        [template, rawRules] = splitOn "\n\n" input
        rules = map parseRule (lines rawRules)

parseRule :: String -> Rule
parseRule line = Rule left right insertion
    where
        [from, to] = splitOn " -> " line
        [left, right] = from
        [insertion] = to