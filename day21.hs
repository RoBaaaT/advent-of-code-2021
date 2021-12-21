import System.IO
    ( hClose, openFile, hGetContents, IOMode(ReadMode) )
import Util ( checkExamples )
import Control.Monad.State (State, get, put, runState)
import qualified Data.Map as Map
import Data.Maybe (isNothing, catMaybes)

main :: IO ()
main = do
    -- check example inputs
    let exampleInput = "Player 1 starting position: 4\nPlayer 2 starting position: 8"
    checkExamples part1 739785 part2 444356092776315 exampleInput

    -- process the real inputs
    handle <- openFile "day21.txt" ReadMode
    input <- hGetContents handle
    print (part1 input)
    print (part2 input)
    hClose handle

part1 :: String -> Int
part1 rawInput = losingScore * finalDiceRolls
    where
        input = parseInput rawInput
        diceRolls = 0
        (gameState, finalDiceRolls) = runState (playTo1000 input) diceRolls
        losingScore = minimum (map snd (fst gameState))

part2 :: String -> Int
part2 rawInput = maximum freqs
    where
        input = parseInput rawInput
        freqs = winningFrequencies $ Map.fromList [(input, 1)]

rollDie :: State DieState Int
rollDie = do
    state <- get
    put (state + 1)
    return (mod state 100 + 1)

-- from https://stackoverflow.com/questions/10133361/haskell-replace-element-in-list/10133429
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, _:b) = splitAt n ls

turn :: GameState -> State DieState GameState
turn gameState = do
    roll1 <- rollDie
    roll2 <- rollDie
    roll3 <- rollDie
    let totalRoll = roll1 + roll2 + roll3
    return $ updateState totalRoll gameState

playTo1000 :: GameState -> State DieState GameState
playTo1000 gameState = do
    newState <- turn gameState
    let (players, _) = newState
    if maximum (map snd players) >= 1000 then
        return newState
    else (do playTo1000 newState)

type DieState = Int
type PlayerState = (Int, Int) -- position, score
type GameState = ([PlayerState], Int) -- player states, player index for next turn

winningFrequencies :: Map.Map GameState Int -> [Int]
winningFrequencies frequencies = if allWon then Map.elems $ Map.fromListWith (+) $ catMaybes winningPlayers else winningFrequencies newFrequencies
    where
        newFrequencies = Map.fromListWith (+) (concatMap evolveState $ Map.assocs frequencies)
        winningPlayers = map (\(state, freq) -> let winner = winningPlayer state in if isNothing winner then Nothing else (let (Just winner') = winner in Just (winner', freq))) (Map.assocs newFrequencies)
        allWon = not (any isNothing winningPlayers)

winningPlayer :: GameState -> Maybe Int
winningPlayer state = if not $ null winningIds then Just $ head winningIds else Nothing
    where
        winningIds = map snd (filter (\(score, _) -> score >= 21) $ zip (map snd $ fst state) [0..])

evolveState :: (GameState, Int) -> [(GameState, Int)]
evolveState (state, freq) = if isNothing $ winningPlayer state then [
    (updateState 3 state, freq),
    (updateState 4 state, freq * 3),
    (updateState 5 state, freq * 6),
    (updateState 6 state, freq * 7),
    (updateState 7 state, freq * 6),
    (updateState 8 state, freq * 3),
    (updateState 9 state, freq)] else [(state, freq)]

updateState :: Int -> GameState -> GameState
updateState totalRoll (players, currentPlayer) = (newPlayers, nextPlayer)
    where
        (currentPos, currentScore) = players !! currentPlayer
        nextPlayer = mod (currentPlayer + 1) (length players)
        newPos = mod ((currentPos - 1) + totalRoll) 10 + 1
        newPlayers = replaceAtIndex currentPlayer (newPos, currentScore + newPos) players

parseInput :: String -> GameState
parseInput input = (map (\line -> (read $ drop prefixLength line, 0)) $ lines input, 0)
    where prefixLength = length "Player X starting position: "
