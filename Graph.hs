module Graph where

import Types
import qualified Data.IntMap as Graph
import Data.Bits
import Data.List

-- Generate graph from list of rules
graphFromRules :: [Rule] -> Graph
graphFromRules = foldr applyRule Graph.empty

-- Add rule to graph
applyRule :: Rule -> Graph -> Graph
applyRule (Border c1 _ c2) g = if Graph.notMember key1 g
                                then Graph.insert key1 (Node c1 [key2]) g
                                else Graph.adjust (addToNode key2) key1 g
                                where   key1 = getKey c1
                                        key2 = getKey c2
applyRule _ g = g

-- Add neighbour to node if it is not yet added
addToNode :: Int -> Node -> Node
addToNode k i@(Node n l) = if not (elem k l)
                            then Node n (k:l)
                            else i

-- Removes node from graph and from list of neighbours from every node
removeNodes :: [Int] -> Graph -> Graph
removeNodes is g = foldr removeNode g is

-- Remove node from graph
removeNode :: Int -> Graph -> Graph
removeNode i g = Graph.map (removeFromNode i) (Graph.delete i g)

-- Remove node from list of neigbours
removeFromNode :: Int -> Node -> Node
removeFromNode i (Node n xs) = Node n (filter (/= i) xs)

-- Convert country name to ID with DJB2 hashing
getKey :: String -> Int
getKey = foldl' (\h c -> 33*h `xor` fromEnum c) 5381