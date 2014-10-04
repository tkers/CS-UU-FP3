module Types where

import qualified Data.IntMap as Graph

data Rule = Border Country Direction Country
          | Size Country Size Country
          | None

type Country = String
data Direction = North | South | East | West | DirFail
data Size = Larger | Smaller | Equal | SizeFail

type Graph = Graph.IntMap Node
data Node = Node String [Int]
data Tree a = Tree a [Tree a]
            | Leaf a
            | Empty

type Path = [Country]

-- Inverts a rule
invRule :: Rule -> Rule
invRule (Border a b c) = Border c (invDirection b) a
invRule (Size a b c) = Size c (invSize b) a
invRule None = None

toDirection :: String -> Direction
toDirection t = case t of
                "North" -> North
                "South" -> South
                "East" -> East
                "West" -> West
                otherwise -> DirFail

-- Inverts direction
invDirection :: Direction -> Direction
invDirection t = case t of
                    North -> South
                    South -> North
                    East -> West
                    West -> East
                    otherwise -> DirFail

toSize t = case t of
            "smaller" -> Smaller
            x | x `elem` ["larger", "bigger"] -> Larger
            x | x `elem` ["large", "big", "small"] -> Equal
            otherwise -> SizeFail

invSize t = case t of
                Larger -> Smaller
                Smaller -> Larger
                Equal -> Equal
                otherwise -> SizeFail

instance Show Rule where
    show t = "(" ++ showRule t ++ ")"

showRule (Border a b c) = a ++ ", " ++ (show b) ++ ", " ++ c
showRule (Size a b c) = a ++ ", " ++ (show b) ++ ", " ++ c
showRule None = "No Rule"

instance Show Direction where
    show t = showDir t

showDir t = case t of
                North -> "North"
                South -> "South"
                East -> "East"
                West -> "West"

instance Show Size where
    show t = showSize t

showSize t = case t of
                Smaller -> "smaller"
                Larger -> "larger"
                Equal -> "equal"

instance Show Node where
    show t = showNode t

showNode (Node a b) = "(" ++ a ++ "," ++ (show b) ++")"

instance Show a => Show (Tree a) where
    show t = showTree t

showTree (Empty)        = "E"
showTree (Leaf a)       = "Leaf " ++ show a
showTree (Tree a bs)    = "Tree " ++ show a ++ "->" ++ show bs