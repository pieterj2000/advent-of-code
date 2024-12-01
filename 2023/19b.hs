import qualified Parser as P
import Control.Applicative (Alternative(..))
import Data.Char (isAlpha)
import Data.Maybe (fromJust)
import Debug.Trace (trace, traceShow)

inputFile = "19a.input"

main = do
    input <- readFile inputFile
    print $ calc input

--calc :: String -> Int
calc input =
    let (ws, pts) = parseInput input
    in sum $ map numPoss $ workFlowPoss ws "in" $ zip "xmas" (repeat (1,4000))

data Workflow = Workflow { name :: String, rules :: [Rule] } deriving (Show)
data Rule = Rule { condition :: Condition, destination :: Dest } | FinalRule { destination :: Dest } deriving (Show)
data Dest = WF { workFlowName :: String } | Accept | Reject deriving (Show, Eq)
data Condition = Condition { property :: Char, comparison :: Comparison, value :: Int } deriving (Show)
data Comparison = Less | Greater deriving (Show, Eq)
type Part = [(Char, Int)]
type Possibilities = [(Char, (Int, Int))]


workflowParser :: P.Parser Workflow
workflowParser =
    let
        ruleP = (\a _ b -> Rule a b) <$> condP <*> P.char ':' <*> destP
        condP = Condition <$> P.anyChar <*> comparisonP <*> P.int
        comparisonP = (\c -> if c == '>' then Greater else Less) <$> P.anyChar
        acceptP = Accept <$ P.char 'A'
        rejectP = Reject <$ P.char 'R'
        wfNameP = WF <$> some (P.satisfy isAlpha)
        destP = acceptP <|> rejectP <|> wfNameP
        finalRuleP = FinalRule <$> destP
    in
        Workflow <$> some (P.satisfy isAlpha) <*> P.inBrackets (P.commaSep (ruleP <|> finalRuleP))

partParser :: P.Parser Part
partParser =
    let propParser = (\a _ b -> (a,b)) <$> P.anyChar <*> P.char '=' <*> P.int
    in P.inBrackets $ P.commaSep propParser

parseInput :: String -> ( [Workflow], [Part] )
parseInput input =  let ls = lines input
                        ( wfs, _:pts ) = break null ls
                    in (map (P.parseResult workflowParser) wfs, map (P.parseResult partParser) pts)

getWorkFlow :: [Workflow] -> String -> Workflow
getWorkFlow [] q = error $ "workflow niet gevonden: " ++ q
getWorkFlow (w:ws) q = if name w == q then w else getWorkFlow ws q

acceptPart :: [Workflow] -> Part -> Bool
acceptPart ws = acceptPart' ws "in"

acceptPart' :: [Workflow] -> String -> Part -> Bool
acceptPart' ws wName p =
    let w = getWorkFlow ws wName
    in case doWorkFlow w p of
            WF next -> acceptPart' ws next p
            Accept -> True
            Reject -> False

satisfiesCond :: Condition -> Part -> Bool
satisfiesCond (Condition prop cond val) part
    | cond == Less = propVal < val
    | cond == Greater = propVal > val
    where propVal = fromJust $ lookup prop part

doWorkFlow :: Workflow -> Part -> Dest
doWorkFlow w p = let
    doe :: [Rule] -> Dest
    doe [] = error "geen rules meer"
    doe ((FinalRule dest):rs) = dest
    doe (r:rs) = if satisfiesCond (condition r) p
                    then destination r
                    else doe rs
    in doe (rules w)

calcScore :: Part -> Int
calcScore = sum . map snd

workFlowPoss :: [Workflow] -> String -> Possibilities -> [Possibilities]
workFlowPoss ws wName poss =
    let w = getWorkFlow ws wName

        doe :: [Rule] -> Possibilities -> [(Possibilities, Dest)]
        doe [] ps = error "geen rules meer"
        doe ((FinalRule dest):_) ps = [(ps, dest)]
        doe (r:rs) ps =
            let (welM, nietM) = splitOnCond (condition r) ps
                rest = case nietM of
                    Just niet -> doe rs niet
                    Nothing   -> []
            in case welM of
                Just wel -> (wel, destination r) : rest
                Nothing  -> rest

        result = doe (rules w) poss
        dones = map fst $ filter ((== Accept) . snd) result
        notdones = filter ((\d -> d /= Reject && d /= Accept) . snd) result
        donotdones = concatMap (\(p, WF wn) -> workFlowPoss ws wn p) notdones

    in dones ++ donotdones

numPoss :: Possibilities -> Int
numPoss = product . map ((\(l,u) -> u - l + 1) . snd)

splitOnCond :: Condition -> Possibilities -> (Maybe Possibilities, Maybe Possibilities)
splitOnCond (Condition prop cond val) poss
    | cond == Less      && val <= l = (Nothing, Just poss)
    | cond == Less      && val > u  = (Just poss, Nothing)
    | cond == Greater   && val >= u = (Nothing, Just poss)
    | cond == Greater   && val < l  = (Just poss, Nothing)
    | otherwise                     = (Just $ update poss prop wel, Just $ update poss prop niet)
    where
        (l, u) = fromJust $ lookup prop poss

        (wel, niet) = if cond == Less
                        then ( (l, val-1), (val, u) )
                        else ( (val+1, u), (l, val) )

        update :: Possibilities -> Char -> (Int, Int) -> Possibilities
        update [] _ _ = error "probeerde te updaten terwijl niet was"
        update ((c, (l, u)):cs) c' (l', u')
            | c /= c' = (c, (l, u)) : update cs c' (l', u')
            | c == c' = (c, (l', u')) : cs
