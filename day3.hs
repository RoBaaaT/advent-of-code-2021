import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Data.Either (rights, fromRight)
import GHC.IO.Exception (assertError)
import Util (parseInt, checkExamples)
import Data.Bits (shiftL)

main :: IO ()
main = do
    -- check example inputs
    let example_inputs = [ "00100"
                         , "11110"
                         , "10110"
                         , "10111"
                         , "10101"
                         , "01111"
                         , "00111"
                         , "11100"
                         , "10000"
                         , "11001"
                         , "00010"
                         , "01010"
                         ]
    checkExamples part1 198 part2 230 example_inputs

    -- process the real inputs
    handle <- openFile "day3.txt" ReadMode
    input <- hGetContents handle
    let ls = lines input
    print (part1 ls)
    print (part2 ls)
    hClose handle

-- GR = Gamma rate, ER = Epsilon rate
data GRERCounter = GRERCounter { zeroes, ones :: [Int] }
data GRER = GRER { gr, er :: Int }

(+++) :: GRERCounter -> GRERCounter -> GRERCounter
(+++) a b = GRERCounter (listAdd (zeroes a) (zeroes b)) (listAdd (ones a) (ones b))

listAdd :: [Int] -> [Int] -> [Int]
listAdd (x:xs) (y:ys) = (x+y) : listAdd xs ys
listAdd [] (y:ys) = y : listAdd [] ys
listAdd (x:xs) [] = x : listAdd xs []
listAdd [] [] = []

part1 :: [String] -> Int
part1 report = gr grer * er grer
    where
        counts = countBitValues report
        grer = calcGRER counts

part2 :: [String] -> Int
part2 report = ogRating report 0 * co2Rating report 0


ogRating :: [String] -> Int -> Int
ogRating [] _ = error "No oxygen rating found"
ogRating [str] _ = bitsToInt (map (== '1') str)
ogRating report level = ogRating newReport (level + 1)
    where
        counts = countBitValues report
        onesMostCommon = (ones counts !! level) >= (zeroes counts !! level)
        newReport = if onesMostCommon then filter (\str -> (str !! level) == '1') report else filter (\str -> (str !! level) == '0') report

co2Rating :: [String] -> Int -> Int
co2Rating [] _ = error "No CO2 rating found"
co2Rating [str] _ = bitsToInt (map (== '1') str)
co2Rating report level = co2Rating newReport (level + 1)
    where
        counts = countBitValues report
        onesLeastCommon = (ones counts !! level) < (zeroes counts !! level)
        newReport = if onesLeastCommon then filter (\str -> (str !! level) == '1') report else filter (\str -> (str !! level) == '0') report

calcGRER :: GRERCounter -> GRER
calcGRER counts = GRER gamma epsilon
    where
        gammaBits = zipWith(<) (zeroes counts) (ones counts)
        gamma = bitsToInt gammaBits
        epsilon = bitsToInt (map not gammaBits)

countBitValues :: [String] -> GRERCounter
countBitValues = foldr ((+++) . countBitValuesSingle) (GRERCounter [] [])

countBitValuesSingle :: String -> GRERCounter
countBitValuesSingle str = GRERCounter (map (\c -> if c == '0' then 1 else 0) str) (map (\c -> if c == '1' then 1 else 0) str)

bitsToInt :: [Bool] -> Int
bitsToInt bits = foldl (\prev (i, bit) -> shiftL (fromEnum bit) i + prev) 0 indexedBits
    where
        indexedBits = zip (reverse [0..(length bits - 1)]) bits