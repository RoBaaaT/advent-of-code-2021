import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Data.Either (rights, fromRight)
import GHC.IO.Exception (assertError)
import Util (parseInt, checkExamples)

main :: IO ()
main = do
    -- check example inputs
    let example_inputs = [ "forward 5"
                         , "down 5"
                         , "forward 8"
                         , "up 3"
                         , "down 8"
                         , "forward 2"
                         ]
    checkExamples part1 150 part2 900 example_inputs

    -- process the real inputs
    handle <- openFile "day2.txt" ReadMode
    input <- hGetContents handle
    let ls = lines input
    print (part1 ls)
    print (part2 ls)
    hClose handle

data Point = Point { x, y :: Int }

part1 :: [String] -> Int
part1 instructions = x dest * y dest
    where dest = followPath (Point 0 0) instructions

part2 :: [String] -> Int
part2 instructions = x dest * y dest
    where (dest, aim) = followPathWithAim (Point 0 0) 0 instructions

followPath :: Point -> [String] -> Point
followPath p [] = p
followPath p (i:is) = case dir of
        "forward" -> followPath (Point (x p + dist) (y p)) is
        "down" -> followPath (Point (x p) (y p + dist)) is
        "up" -> followPath (Point (x p) (y p - dist)) is
        _ -> p
    where
        [dir, num] = words i
        dist = fromRight 0 (parseInt num)

followPathWithAim :: Point -> Int -> [String] -> (Point, Int)
followPathWithAim p a [] = (p, a)
followPathWithAim p a (i:is) = case dir of
        "forward" -> followPathWithAim (Point (x p + dist) (y p + dist * a)) a is
        "down" -> followPathWithAim p (a + dist) is
        "up" -> followPathWithAim p (a - dist) is
        _ -> (p, a)
    where
        [dir, num] = words i
        dist = fromRight 0 (parseInt num)
