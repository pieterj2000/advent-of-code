import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sortOn, elemIndex, nub, sort, foldl')
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Sequence as Seq

import Debug.Trace

inputFile = "25a.input"

main = do
    input <- readFile inputFile
    print $ calc input

--calc :: String -> Int
--calc = parseInput
calc input = 
    let graph = parseInput input 
        combos = pairs . S.toList . fst $ graph
        in head $ mapMaybe (testAndCalc graph) combos

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs


data Node = Node Int String deriving (Eq, Show)
type Nodes = S.Set Node

instance Ord Node where
    compare (Node i _) (Node j _) = compare i j

type Edge = (Node, Node)
type Edges = M.Map Node Nodes

type Graph = (Nodes, Edges)

addEdge :: Edge -> Edges -> Edges
addEdge (a,b) = M.insertWith S.union a (S.singleton b) . M.insertWith S.union b (S.singleton a)

removeEdge :: Edge -> Edges -> Edges
removeEdge (a,b) = M.adjust (S.delete b) a . M.adjust (S.delete a) b

removeEdges ::Foldable t => Edges -> t Edge -> Edges
removeEdges = foldr removeEdge

parseInput :: String -> Graph
parseInput input =
    let ls = lines input
        doLine l = (take 3 l, words $ drop 4 l)
        edgeString = map doLine ls
        nodesString = sort . nub $ concatMap (uncurry (:)) edgeString
        getId name = fromJust $ elemIndex name nodesString
        getNode name = Node (getId name) name

        processEdge (name, neighbours) edges =
            let n = getNode name
                neighbs = map (((,) n) . getNode) neighbours
                newEdges = foldr addEdge edges neighbs
            in newEdges

        edges = foldr processEdge M.empty edgeString
        nodes = S.fromList $ map getNode nodesString

    in (nodes, edges)


getPath :: Graph -> Node -> Node -> Maybe [Edge]
getPath (nodes, edges) n1 n2 =
    let bfs :: Queue -> Nodes -> Maybe [Edge]
        bfs queue seen
            | isEmpty queue = Nothing
            | n == n2       = Just newPath
            | otherwise     = bfs newQueue newSeen
            where
                (n, path, restQueue) = dequeue queue
                (_, prevNode) = head path
                newPath
                    | n == n1       = []
                    | path == []    = [(n1,n)]
                    | otherwise     = (prevNode,n):path

                newNeighs = (edges M.! n) S.\\ seen
                newSeen = S.insert n seen

                newQueue = enqueueList restQueue $ S.map (\neighbour -> (neighbour, newPath)) newNeighs

    in  bfs (enqueue emptyQueue (n1, [(n1, n1)])) S.empty

reachable :: Graph -> Node -> Nodes
reachable (nodes, edges) start =
    let go :: Nodes -> Node -> Nodes
        go seen n =
            let newNeighs = (edges M.! n) S.\\ seen
                newSeen = S.insert n seen
            in foldl' go newSeen newNeighs
    in go S.empty start

testAndCalc :: Graph -> (Node, Node) -> Maybe Int
testAndCalc (nodes, edges1) (n1,n2) = do

    path1 <- getPath (nodes, edges1) n1 n2

    let edges2 = removeEdges edges1 path1
    path2 <- getPath (nodes, edges2) n1 n2

    let edges3 = removeEdges edges2 path2
    path3 <- getPath (nodes, edges3) n1 n2

    --traceShowM path1
    --traceShowM path2
    --traceShowM path3

    let edges4 = removeEdges edges3 path3
    case getPath (nodes, edges4) n1 n2 of
        Just _ -> Nothing
        Nothing ->
            let haalbaar = reachable (nodes, edges4) n1
                helft1 = S.size haalbaar
                helft2 = S.size nodes - helft1
            in Just (helft1*helft2)

printEdges (_, edges) =
    let showNode (Node i n) = n
    in mapM_ putStrLn $ map (\(s1,s2) -> (showNode s1) ++ " -> " ++ (showNode s2) ) $ concatMap (\(node, nodes) -> map ((,) node) $ S.toList nodes) $ M.toList edges






----------
type Queue = Seq.Seq (Node, [Edge])

emptyQueue :: Queue
emptyQueue = Seq.empty

enqueue :: Queue -> (Node, [Edge]) -> Queue
enqueue = (Seq.:|>)

dequeue :: Queue -> (Node, [Edge], Queue)
dequeue q
    | isEmpty q = error "dequeueing empty queue"
    | otherwise = let ((a,b) Seq.:< rest) = Seq.viewl q in (a,b, rest)

isEmpty :: Queue -> Bool
isEmpty = Seq.null

enqueueList :: Foldable t => Queue -> t (Node, [Edge]) -> Queue
enqueueList = foldr (flip enqueue)