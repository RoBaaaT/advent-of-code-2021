import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Util ( checkExamples )
import Data.Char (isSpace)
import Control.Monad.State (runState, State, evalState, MonadState (get, put), replicateM, gets)

main :: IO ()
main = do
    -- check example inputs
    let exampleInputs = [ "8A004A801A8002F478", "620080001611562C8802118E34", "C0015000016115A2E0802F182340", "A0016C880162017C3686B18A3D4780" ]
    let exampleResults = [ (16, 15), (12, 46), (23, 46), (31, 54) ]
    mapM_ (\(i, (r1, r2)) -> checkExamples part1 r1 part2 r2 i) (zip exampleInputs exampleResults)

    -- process the real inputs
    handle <- openFile "day16.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)

    hClose handle

part1 :: String -> Int
part1 rawInput = sumVersions packet
    where
        parser = parseInput rawInput
        packet = evalState parsePacket parser

part2 :: String -> Int
part2 rawInput = eval packet
    where
        parser = parseInput rawInput
        packet = evalState parsePacket parser

sumVersions :: Packet -> Int
sumVersions (version, Literal _) = version
sumVersions (version, Operator (_, subPackets)) = version + sum (map sumVersions subPackets)

eval :: Packet -> Int
eval (_, Literal value) = value
eval (_, Operator (0, xs)) = sum (map eval xs)
eval (_, Operator (1, xs)) = product (map eval xs)
eval (_, Operator (2, xs)) = minimum (map eval xs)
eval (_, Operator (3, xs)) = maximum (map eval xs)
eval (_, Operator (5, [first, second])) = if eval first > eval second then 1 else 0
eval (_, Operator (6, [first, second])) = if eval first < eval second then 1 else 0
eval (_, Operator (7, [first, second])) = if eval first == eval second then 1 else 0
eval (_, Operator (tid, xs)) = error $ "invalid packet type " ++ show tid

-- packet parsing
type Packet = (Int, PacketData) -- (version, data)
type OperatorData = (Int, [Packet])
data PacketData = Literal Int | Operator OperatorData
    deriving Show

type PacketParser = ([Bool], Int, Int)
offset :: PacketParser -> Int
offset (_, o, _) = o

takeBit :: State PacketParser Bool
takeBit = do
    (packet, offset, size) <- get
    if size - 1 > offset then (do
        let bit = packet !! offset
        put (packet, offset + 1, size)
        return bit)
    else error ("exceeded size taking a bit at " ++ show offset)

takeBits :: Int -> State PacketParser Int
takeBits bitCount = do
    (packet, offset, size) <- get
    if size - bitCount > offset then (do
        let num = foldl (\acc i -> acc * 2 + if packet !! i then 1 else 0) 0 [offset..offset + bitCount - 1]
        put (packet, offset + bitCount, size)
        return num)
    else error ("exceeded size taking " ++ show bitCount ++ " bits at " ++ show offset)

parseLiteral :: State PacketParser Int
parseLiteral = go 0
    where
        go i = do
            prefix <- takeBit
            bits <- takeBits 4
            let i' = i * 16 + bits
            if prefix then go i' else return i'

parseSubPacketsWithLength :: Int -> State PacketParser [Packet]
parseSubPacketsWithLength len = do
    if len == 0 then return []
    else if len > 0 then (do
        currentOffset <- gets offset
        packet <- parsePacket
        newOffset <- gets offset
        otherPackets <- parseSubPacketsWithLength (len - (newOffset - currentOffset))
        return (packet : otherPackets))
    else error $ "invalid sub packets length " ++ show len

parseSubPackets :: State PacketParser [Packet]
parseSubPackets = do
    lengthTypeId <- takeBit
    if lengthTypeId then (do
        subPacketsCount <- fromIntegral <$> takeBits 11
        replicateM subPacketsCount parsePacket)
    else (do
        subPacketsLength <- fromIntegral <$> takeBits 15
        parseSubPacketsWithLength subPacketsLength)

parsePacket :: State PacketParser Packet
parsePacket = do
    version <- takeBits 3
    t <- takeBits 3
    case t of
        4 -> do
                literal <- parseLiteral
                return (version, Literal literal)
        id -> do
                subPackets <- parseSubPackets
                return (version, Operator (id, subPackets))

-- input parsing
parseInput :: String -> PacketParser
parseInput input = (concatMap decodeHex safeInput, 0, 4 * length safeInput)
    where
        safeInput = rstrip input

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

decodeHex :: Char -> [Bool]
decodeHex '0' = [False,False,False,False]
decodeHex '1' = [False,False,False,True]
decodeHex '2' = [False,False,True,False]
decodeHex '3' = [False,False,True,True]
decodeHex '4' = [False,True,False,False]
decodeHex '5' = [False,True,False,True]
decodeHex '6' = [False,True,True,False]
decodeHex '7' = [False,True,True,True]
decodeHex '8' = [True,False,False,False]
decodeHex '9' = [True,False,False,True]
decodeHex 'A' = [True,False,True,False]
decodeHex 'B' = [True,False,True,True]
decodeHex 'C' = [True,True,False,False]
decodeHex 'D' = [True,True,False,True]
decodeHex 'E' = [True,True,True,False]
decodeHex 'F' = [True,True,True,True]
decodeHex a = error "not a hex character"