import qualified Parser as P
import Data.Char (isAlpha)
import Control.Applicative (many)
import qualified Data.Vector as V
import qualified Data.Sequence as S
import Data.Sequence ( Seq((:|>)), ViewL (..) )
import Data.Vector ((!), (//))
import Data.List (foldl')
import Debug.Trace (traceShow, trace)

inputFile = "20a.input"

main = do
    input <- readFile inputFile
    print $ calc input

--calc :: String -> Int
calc = calcScore . nTimes 1000 (doAllSignals . pressButton) . buildDing . parseInput

calcScore :: State -> Int
calcScore (_,_,Queue _ cH cL) = cH*cL

nTimes :: Int -> (a -> a) -> (a -> a)
nTimes n f = foldr1 (.) (replicate n f)

pressButton :: State -> State
pressButton (modules, states, queue) = (modules, states, enqueue queue (-1, Low, 0))

doAllSignals :: State -> State
doAllSignals s@(modules, states, queue)
    | isEmpty queue = s
    | otherwise = doAllSignals $ handleSignal s

handleSignal :: State -> State
handleSignal (modules, states, queue) =
    let (sig, newqueue) = dequeue queue
    in -- trace (show sig ++ show (let (a,b,c) = sig in states ! c)) $ 
        sendTo (modules, states, newqueue) sig

data Signal = Low | High deriving (Eq, Show)
data Module a = Broadcaster [a] a | FlipFlop [a] a | Conjunction [a] [a] a deriving (Show, Eq)
type State = (V.Vector (Module Int), V.Vector Signal, Queue)

flipSig :: Signal -> Signal
flipSig Low = High
flipSig High = Low

name :: Module a -> a
name (Broadcaster _ a) = a
name (FlipFlop _ a) = a
name (Conjunction _ _ a) = a

destinationModules :: Module a -> [a]
destinationModules (Broadcaster a _) = a
destinationModules (FlipFlop a _) = a
destinationModules (Conjunction _ a _) = a

sendTo :: State -> (Int, Signal, Int) -> State
sendTo (modules, states, queue) (srcIndex, signal, rcvIndex) =
    let m = modules ! rcvIndex
        state = states ! rcvIndex
        sends = destinationModules m
    in case m of
        (Broadcaster sends _) -> (modules, states, enqueueList queue $ map (\i -> (rcvIndex,signal,i)) sends)
        (FlipFlop sends _ )
            | signal == High -> (modules, states, queue)
            | signal == Low -> (modules, states // [(rcvIndex, flipSig state)], enqueueList queue $ map (\i -> (rcvIndex,flipSig state,i)) sends)
        (Conjunction recvs sends _) ->
            let recvstates = map (states !) recvs
                sendSignal = if all (==High) recvstates then Low else High
            in (modules,
                states // [(rcvIndex, sendSignal)],
                enqueueList queue $ map (\i -> (rcvIndex,sendSignal,i)) sends)



moduleP :: P.Parser (String, [String])
moduleP = (,) <$> ((:) <$> P.anyChar <*> P.word) <*> (P.string " -> " *> P.separatedBy (P.string ", ") P.word)

parseInput :: String -> [Module String]
parseInput = map (doe . P.parseResult moduleP) . lines
    where
        doe :: (String, [String]) -> Module String
        doe (name, uitlijst)
            | head name == 'b' = Broadcaster uitlijst name
            | head name == '%' = FlipFlop uitlijst (tail name)
            | head name == '&' = Conjunction [] uitlijst (tail name)

buildDing :: [Module String] -> State
buildDing input = (modules, state, emptyQueue)
    where
        numModules = length input
        modulesString = V.fromList input
        modules = V.imap trans modulesString

        trans :: Int -> Module String -> Module Int
        trans index modul =
            let toIndex :: String -> Int
                toIndex n = case V.findIndex (\m -> name m == n) modulesString of
                    Nothing -> -1
                    Just i -> i
            in case modul of
            Broadcaster sends _ -> Broadcaster (map toIndex sends) 0
            FlipFlop sends _ -> FlipFlop (map toIndex sends) index
            Conjunction _ sends _ ->
                    let mName = name modul
                        recvs = filter (\otherm -> mName `elem` (destinationModules otherm)) input
                    in Conjunction (map (toIndex . name) recvs) (map toIndex sends) index

        state = V.generate numModules initialState

        initialState :: Int -> Signal
        initialState i = case modules ! i of
            Broadcaster sends _ -> Low
            FlipFlop sends _ -> Low
            Conjunction recvs sends _ -> High




----------
data Queue = Queue { queue :: S.Seq (Int, Signal, Int), countHigh :: Int, countLow :: Int } deriving (Show)
emptyQueue :: Queue
emptyQueue = Queue S.empty 0 0

enqueue :: Queue -> (Int, Signal, Int) -> Queue
enqueue (Queue s cH cL) (_,High,-1)  = Queue s (cH+1) cL
enqueue (Queue s cH cL) (_,Low,-1)  = Queue s cH (cL+1)
enqueue (Queue s cH cL) (src,High,dest) = Queue (s :|> (src,High,dest)) (cH+1) cL
enqueue (Queue s cH cL) (src,Low,dest) = Queue (s :|> (src,Low,dest)) cH (cL+1)

dequeue :: Queue -> ((Int, Signal, Int), Queue)
dequeue q@(Queue s cH cL)
    | isEmpty q = error "dequeueing empty queue"
    | otherwise = let (a :< rest) = S.viewl s in (a, Queue rest cH cL)

isEmpty :: Queue -> Bool
isEmpty (Queue s cH cL) = S.null s

enqueueList :: Queue -> [(Int, Signal, Int)] -> Queue
enqueueList = foldl' enqueue