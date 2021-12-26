import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Util ( checkExamples )
import Data.Array ( Array, array, elems, (//), Ix (range, inRange), bounds )
import Data.List.Split (splitOn)

main :: IO ()
main = do
    -- check example inputs
    let exampleInput = "on x=-5..47,y=-31..22,z=-19..33\n\
\on x=-44..5,y=-27..21,z=-14..35\n\
\on x=-49..-1,y=-11..42,z=-10..38\n\
\on x=-20..34,y=-40..6,z=-44..1\n\
\off x=26..39,y=40..50,z=-2..11\n\
\on x=-41..5,y=-41..6,z=-36..8\n\
\off x=-43..-33,y=-45..-28,z=7..25\n\
\on x=-33..15,y=-32..19,z=-34..11\n\
\off x=35..47,y=-46..-34,z=-11..5\n\
\on x=-14..36,y=-6..44,z=-16..29\n\
\on x=-57795..-6158,y=29564..72030,z=20435..90618\n\
\on x=36731..105352,y=-21140..28532,z=16094..90401\n\
\on x=30999..107136,y=-53464..15513,z=8553..71215\n\
\on x=13528..83982,y=-99403..-27377,z=-24141..23996\n\
\on x=-72682..-12347,y=18159..111354,z=7391..80950\n\
\on x=-1060..80757,y=-65301..-20884,z=-103788..-16709\n\
\on x=-83015..-9461,y=-72160..-8347,z=-81239..-26856\n\
\on x=-52752..22273,y=-49450..9096,z=54442..119054\n\
\on x=-29982..40483,y=-108474..-28371,z=-24328..38471\n\
\on x=-4958..62750,y=40422..118853,z=-7672..65583\n\
\on x=55694..108686,y=-43367..46958,z=-26781..48729\n\
\on x=-98497..-18186,y=-63569..3412,z=1232..88485\n\
\on x=-726..56291,y=-62629..13224,z=18033..85226\n\
\on x=-110886..-34664,y=-81338..-8658,z=8914..63723\n\
\on x=-55829..24974,y=-16897..54165,z=-121762..-28058\n\
\on x=-65152..-11147,y=22489..91432,z=-58782..1780\n\
\on x=-120100..-32970,y=-46592..27473,z=-11695..61039\n\
\on x=-18631..37533,y=-124565..-50804,z=-35667..28308\n\
\on x=-57817..18248,y=49321..117703,z=5745..55881\n\
\on x=14781..98692,y=-1341..70827,z=15753..70151\n\
\on x=-34419..55919,y=-19626..40991,z=39015..114138\n\
\on x=-60785..11593,y=-56135..2999,z=-95368..-26915\n\
\on x=-32178..58085,y=17647..101866,z=-91405..-8878\n\
\on x=-53655..12091,y=50097..105568,z=-75335..-4862\n\
\on x=-111166..-40997,y=-71714..2688,z=5609..50954\n\
\on x=-16602..70118,y=-98693..-44401,z=5197..76897\n\
\on x=16383..101554,y=4615..83635,z=-44907..18747\n\
\off x=-95822..-15171,y=-19987..48940,z=10804..104439\n\
\on x=-89813..-14614,y=16069..88491,z=-3297..45228\n\
\on x=41075..99376,y=-20427..49978,z=-52012..13762\n\
\on x=-21330..50085,y=-17944..62733,z=-112280..-30197\n\
\on x=-16478..35915,y=36008..118594,z=-7885..47086\n\
\off x=-98156..-27851,y=-49952..43171,z=-99005..-8456\n\
\off x=2032..69770,y=-71013..4824,z=7471..94418\n\
\on x=43670..120875,y=-42068..12382,z=-24787..38892\n\
\off x=37514..111226,y=-45862..25743,z=-16714..54663\n\
\off x=25699..97951,y=-30668..59918,z=-15349..69697\n\
\off x=-44271..17935,y=-9516..60759,z=49131..112598\n\
\on x=-61695..-5813,y=40978..94975,z=8655..80240\n\
\off x=-101086..-9439,y=-7088..67543,z=33935..83858\n\
\off x=18020..114017,y=-48931..32606,z=21474..89843\n\
\off x=-77139..10506,y=-89994..-18797,z=-80..59318\n\
\off x=8476..79288,y=-75520..11602,z=-96624..-24783\n\
\on x=-47488..-1262,y=24338..100707,z=16292..72967\n\
\off x=-84341..13987,y=2429..92914,z=-90671..-1318\n\
\off x=-37810..49457,y=-71013..-7894,z=-105357..-13188\n\
\off x=-27365..46395,y=31009..98017,z=15428..76570\n\
\off x=-70369..-16548,y=22648..78696,z=-1892..86821\n\
\on x=-53470..21291,y=-120233..-33476,z=-44150..38147\n\
\off x=-93533..-4276,y=-16170..68771,z=-104985..-24507"
    checkExamples part1 474140 part2 2758514936282235 exampleInput

    -- process the real inputs
    handle <- openFile "day22.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)
    hClose handle

part1 :: String -> Int
part1 rawInput = length $ filter (== True) $ elems finalReactor
    where
        input = parseInput rawInput
        reactor = array ((-50, -50, -50), (50, 50, 50)) [((x, y, z), False) | x <- [-50..50], y <- [-50..50], z <- [-50..50]]
        finalReactor = initializeReactorN reactor input

part2 :: String -> Int
part2 rawInput = sum $ map volume finalReactor
    where
        input = parseInput rawInput
        finalReactor = initializeReactor [] input

-- naive solution (part 1)
type Reactor = Array (Int, Int, Int) Bool

initializeReactorN :: Reactor -> [InitializationStep] -> Reactor
initializeReactorN = foldl executeInitializationStepN

executeInitializationStepN :: Reactor -> InitializationStep -> Reactor
executeInitializationStepN reactor (on, (fromX, fromY, fromZ), (toX, toY, toZ)) = (//) reactor $ zip (range (from, to)) (repeat on)
    where
        ((minX, minY, minZ), (maxX, maxY, maxZ)) = bounds reactor
        from = (max minX fromX, max minY fromY, max minZ fromZ)
        to = (min maxX toX, min maxY toY, min maxZ toZ)

-- more efficient solution (part 2)
type AABB = ((Int, Int, Int), (Int, Int, Int))

volume :: AABB -> Int
volume ((fromX, fromY, fromZ), (toX, toY, toZ)) = (toX - fromX + 1) * (toY - fromY + 1) * (toZ - fromZ + 1)

-- returns a list of AABBs which are the parts of a that do not intersect with b
subtractAABB :: AABB -> AABB -> [AABB]
subtractAABB ((ax, ay, az), (aX, aY, aZ)) ((bx, by, bz), (bX, bY, bZ))
    | (bX < ax || bx > aX) || (bY < ay || by > aY) || (bZ < az || bz > aZ) = [((ax, ay, az), (aX, aY, aZ))] -- no intersection
    | bx <= ax && bX >= aX && by <= ay && bY >= aY && bz <= az && bZ >= aZ = [] -- a fully inside b
    | ax < bx && aX > bX && ay < by && aY > bY && az < bz && aZ > bZ = [
        ((ax, ay, az), (aX, aY, bz - 1)),
        ((ax, ay, bZ + 1), (aX, aY, aZ)),
        ((ax, ay, bz), (aX, by - 1, bZ)),
        ((ax, bY + 1, bz), (aX, aY, bZ)),
        ((ax, by, bz), (bx - 1, bY, bZ)),
        ((bX + 1, by, bz), (aX, bY, bZ))] -- b fully inside a
    | otherwise = [((fromX, fromY, fromZ), (toX, toY, toZ)) | (fromX, toX) <- xInRanges, (fromY, toY) <- yInRanges, (fromZ, toZ) <- zInRanges] ++
            [((fromX, fromY, fromZ), (toX, toY, toZ)) | (fromX, toX) <- xOutRanges, (fromY, toY) <- yInRanges, (fromZ, toZ) <- zInRanges] ++
            [((fromX, fromY, fromZ), (toX, toY, toZ)) | (fromX, toX) <- xInRanges, (fromY, toY) <- yOutRanges, (fromZ, toZ) <- zInRanges] ++
            [((fromX, fromY, fromZ), (toX, toY, toZ)) | (fromX, toX) <- xInRanges, (fromY, toY) <- yInRanges, (fromZ, toZ) <- zOutRanges] ++
            [((fromX, fromY, fromZ), (toX, toY, toZ)) | (fromX, toX) <- xOutRanges, (fromY, toY) <- yInRanges, (fromZ, toZ) <- zOutRanges] ++
            [((fromX, fromY, fromZ), (toX, toY, toZ)) | (fromX, toX) <- xOutRanges, (fromY, toY) <- yOutRanges, (fromZ, toZ) <- zInRanges] ++
            [((fromX, fromY, fromZ), (toX, toY, toZ)) | (fromX, toX) <- xInRanges, (fromY, toY) <- yOutRanges, (fromZ, toZ) <- zOutRanges]
        where
            (xInRanges, xOutRanges) = subtractCoords (ax, aX) (bx, bX)
            (yInRanges, yOutRanges) = subtractCoords (ay, aY) (by, bY)
            (zInRanges, zOutRanges) = subtractCoords (az, aZ) (bz, bZ)

subtractCoords :: (Int, Int) -> (Int, Int) -> ([(Int, Int)], [(Int, Int)])
subtractCoords (fromA, toA) (fromB, toB)
    | fromA < fromB && toA > toB = ([(fromA, fromB - 1), (toB + 1, toA)], [(fromB, toB)]) -- b fully inside a
    | toB < fromA = ([(fromA, toA)], []) -- b left of a
    | fromB > toA = ([(fromA, toA)], []) -- b right of a
    | fromB <= fromA && toB >= toA = ([], [(fromA, toA)]) -- a fully inside b
    | fromB <= fromA && toB < toA = ([(toB + 1, toA)], [(fromA, toB)]) -- b intersects a from left
    | fromB > fromA && toB >= toA = ([(fromA, fromB - 1)], [(fromB, toA)]) -- b intersects a from right
    | otherwise = error "unhandled intersection"

initializeReactor :: [AABB] -> [InitializationStep] -> [AABB]
initializeReactor = foldl executeInitializationStep

executeInitializationStep :: [AABB] -> InitializationStep -> [AABB]
executeInitializationStep reactor (True, from, to) = reactor ++ addition
    where
        addition = foldl (\new reactorPart -> concatMap (`subtractAABB` reactorPart) new) [(from, to)] reactor
executeInitializationStep reactor (False, from, to) = concatMap (`subtractAABB` (from, to)) reactor

type InitializationStep = (Bool, (Int, Int, Int), (Int, Int, Int))

parseInput :: String -> [InitializationStep]
parseInput input = map parseInitializationStep $ lines input

parseInitializationStep :: String -> InitializationStep
parseInitializationStep ('o':'n':' ':str) = (True, from, to)
    where (from, to) = parseRange str
parseInitializationStep ('o':'f':'f':' ':str) = (False, from, to)
    where (from, to) = parseRange str
parseInitializationStep _ = error "invalid initialization step"

parseRange :: String -> ((Int, Int, Int), (Int, Int, Int))
parseRange input = ((fromX, fromY, fromZ), (toX, toY, toZ))
    where
        coords = splitOn "," input
        [(fromX, toX), (fromY, toY), (fromZ, toZ)] = map parseCoordRange coords

parseCoordRange :: String -> (Int, Int)
parseCoordRange input = (from, to)
    where
        [_, range] = splitOn "=" input
        [from, to] = map read $ splitOn ".." range