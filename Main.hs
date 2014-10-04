module Main where

import qualified Data.IntMap as Graph
import Graph
import Parse
import Types

main :: IO ()
main = do
        putStrLn "Path to description file:"
        path <- getLine
        file <- readFile path
        let rules = extractRules ((concat . lines) file)
            graph = graphFromRules rules
        putStrLn "Do you want to see the graph? y/n"
        yn1 <- getLine
        if yn1 == "y"
            then do putStrLn (Graph.showTree graph)
            else do putStrLn ""
        putStrLn "Do you want to calculate a shortest path? y/n"
        yn2 <- getLine
        if yn2 == "y"
            then do putStrLn "From?"
                    from1 <- getLine
                    if countryExists graph from1
                        then do putStrLn "To?"
                                to1 <- getLine
                                if countryExists graph to1
                                    then do let path = shortestPath from1 to1 graph
                                            putStrLn ("Shortest path: " ++ show path)
                                    else do putStrLn "Country does not exist!"
                        else do putStrLn "Country does not exist!"
            else do putStrLn ""
        putStrLn "Do you want to calculate a minimum spanning tree? y/n"
        yn3 <- getLine
        if yn3 == "y"
            then do putStrLn "From?"
                    from2 <- getLine
                    if countryExists graph from2
                        then do let mst = minimumSpanningTree from2 graph
                                putStrLn ("Minimum spanning tree from " ++ from2 ++ ":")
                                putStrLn (show mst)
                        else do putStrLn "Country does not exist!"
            else do putStrLn ""
        return ()

-- Returns shortest path between 2 countries
shortestPath :: Country -> Country -> Graph -> Path
shortestPath from to g =    if (not.null) paths
                                then reverse (head paths)
                                else []
                            where
                                paths = shortestPath' [[from]] to g

-- Breadth-first search for country from given [Path]
shortestPath' :: [Path] -> Country -> Graph -> [Path]
shortestPath' from to g =   if (not.null) a
                                then a
                                else if Graph.null g
                                        then []
                                        else shortestPath' next to g'
                            where
                                a = filter (\x -> head x == to) from
                                next = concatMap (\x -> nextLevel x to g) from
                                g' = removeNodes (map (getKey.head) from) g

-- Returns all possible paths following from a given path
nextLevel :: Path -> Country -> Graph -> [Path]
nextLevel from to g =   map ((flip (:)) from) (map (getCountryName g) dests)
                        where
                            Just (Node _ dests) = Graph.lookup (getKey (head from)) g

-- Returns minimum spanning tree
minimumSpanningTree :: Country -> Graph -> Tree Country
minimumSpanningTree c g =   buildTree (Leaf c) g [c] [(c, getNode g x) | x <- l]
                            where
                                Node _ l = getNode g (getKey c)

-- Adds nodes one by one to MST
buildTree :: Tree Country -> Graph -> [Country] -> [(Country, Node)] -> Tree Country
buildTree t _ _ [] = t
buildTree t g cs ((par, (Node n l)):ns) = if n `elem` cs
                                            then buildTree t g cs ns
                                            else buildTree (addNode t par n) g (n:cs) (ns ++ l')
                                        where
                                            l' = [(n, getNode g x) | x <- l]

-- Adds single node to tree at given position
addNode :: Tree Country -> Country -> Country -> Tree Country
addNode t@(Leaf n) p c = if n == p
                            then Tree n [Leaf c]
                            else t
addNode t@(Tree n l) p c =  if n == p
                                then Tree n ((Leaf c):l)
                                else Tree n (map (\x -> addNode x p c) l)
-- Returns country name from country ID
getCountryName g i = n
                     where
                        Just (Node n _) = Graph.lookup i g

-- Returns node from country name
getNode g n = a
                where
                    Just a = Graph.lookup n g

-- Returns whether given country name exists in graph
countryExists :: Graph -> Country -> Bool
countryExists g c = case n of
                        Just _ -> True
                        otherwise -> False
                    where
                        n = Graph.lookup (getKey c) g