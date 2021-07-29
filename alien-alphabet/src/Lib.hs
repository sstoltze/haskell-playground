module Lib where

import Data.List (nub)

import Text.Printf

data Node a = Node { nodeName :: a
                   } deriving (Eq, Show)

data Edge a = Edge { edgeStart :: Node a
                   , edgeEnd :: Node a
                   } deriving (Eq, Show)

data Digraph a = Digraph { digraphEdges :: [Edge a]
                         } deriving (Eq, Show)

digraphStartNodes :: (Eq a) => Digraph a -> [Node a]
digraphStartNodes = nub . foldr ((:) . edgeStart) [] . digraphEdges

digraphEndNodes :: (Eq a) => Digraph a -> [Node a]
digraphEndNodes = nub . foldr ((:) . edgeEnd) [] . digraphEdges

digraphNodes :: (Eq a) => Digraph a -> [Node a]
digraphNodes g = digraphNodes' (digraphEdges g) []
  where
    digraphNodes' [] nodes = nub nodes
    digraphNodes' (e:es) nodes = digraphNodes' es (edgeStart e : edgeEnd e : nodes)

digraphDot :: (PrintfArg a) => Digraph a -> String
digraphDot g = "digraph { \n" ++ concatMap edgeDot (digraphEdges g) ++ " }"
  where
    edgeDot e = printf "  \"%v\" -> \"%v\" \n" (nodeName $ edgeStart e) (nodeName $ edgeEnd e)

testDigraph :: Digraph Int
testDigraph = Digraph { digraphEdges = [Edge (Node 1) (Node 2), Edge (Node 2) (Node 4), Edge (Node 1) (Node 4)] }

digraphFromAlphabeticalOrder :: [String] -> Digraph Char
digraphFromAlphabeticalOrder ws = digraphFromAlphabeticalOrder' ws (Digraph [])
  where
    digraphFromAlphabeticalOrder' [] g = g
    digraphFromAlphabeticalOrder' [_] g = g
    digraphFromAlphabeticalOrder' (x:y:xs) g = digraphFromAlphabeticalOrder' (y:xs) (updateDigraph x y g)
    updateDigraph x y g = case compareStrings x y of
      Just (a,b) -> g { digraphEdges = Edge (Node a) (Node b) : digraphEdges g }
      Nothing -> g
    compareStrings [] _ = Nothing
    compareStrings (a:as) (b:bs) = if a == b then compareStrings as bs else Just (a, b)
    compareStrings _ _ = Nothing
