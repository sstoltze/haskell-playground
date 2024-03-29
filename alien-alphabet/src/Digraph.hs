module Digraph where

import           Data.List     (nub)
import           Data.Text     (Text, pack)
import           Data.Text.ICU (LocaleName (Root), breakCharacter, breaks,
                                brkBreak)
import           Text.Printf   (PrintfArg, printf)

newtype Node a = Node
  { nodeName :: a
  }
  deriving (Eq, Show)

type Colour = String

data Edge a = Edge
  { edgeStart  :: Node a,
    edgeEnd    :: Node a,
    edgeColour :: Maybe Colour
  }
  deriving (Show)

mkEdge :: a -> a -> Edge a
mkEdge n1 n2 = Edge (Node n1) (Node n2) Nothing

setEdgeColour :: Colour -> Edge a -> Edge a
setEdgeColour c e = e {edgeColour = Just c}

removeEdgeColour :: Edge a -> Edge a
removeEdgeColour e = e {edgeColour = Nothing}

edgeDot :: (PrintfArg a) => Edge a -> String
edgeDot e = printf "  \"%v\" -> \"%v\"%v" (nodeName $ edgeStart e) (nodeName $ edgeEnd e) (case edgeColour e of Just c -> " [color=" ++ c ++ "]"; _ -> "")

-- Two paths are equal if they start and end at the same node, independent of colour
edgeEq :: (Eq a) => Edge a -> Edge a -> Bool
edgeEq e1 e2 = edgeStart e1 == edgeStart e2 && edgeEnd e1 == edgeEnd e2

instance Eq a => Eq (Edge a) where
  (==) = edgeEq

type Path a = [Edge a]

pathStart :: Path a -> Node a
pathStart p = edgeStart $ head p

pathEnd :: Path a -> Node a
pathEnd p = edgeEnd $ last p

newtype Digraph a = Digraph
  { digraphEdges :: [Edge a]
  }
  deriving (Eq, Show)

testDigraph :: Digraph Int
testDigraph = Digraph {digraphEdges = [mkEdge 1 2, setEdgeColour "red" (mkEdge 2 4), setEdgeColour "blue" (mkEdge 1 4)]}

digraphStartNodes :: (Eq a) => Digraph a -> [Node a]
digraphStartNodes = nub . foldr ((:) . edgeStart) [] . digraphEdges

digraphEndNodes :: (Eq a) => Digraph a -> [Node a]
digraphEndNodes = nub . foldr ((:) . edgeEnd) [] . digraphEdges

digraphNodes :: (Eq a) => Digraph a -> [Node a]
digraphNodes g = digraphNodes' (digraphEdges g) []
  where
    digraphNodes' [] nodes = nub nodes
    digraphNodes' (e : es) nodes = digraphNodes' es (edgeStart e : edgeEnd e : nodes)

digraphDot :: (PrintfArg a) => Digraph a -> String
digraphDot g = "digraph {\n" ++ concatMap ((++ "\n") . edgeDot) (digraphEdges g) ++ "}"

digraphColourPath :: (Eq a) => Colour -> Path a -> Digraph a -> Digraph a
digraphColourPath _ [] g = g
digraphColourPath c (e : es) g = digraphColourPath c es $ g {digraphEdges = updateEdgeColour e $ digraphEdges g}
  where
    updateEdgeColour _ [] = []
    updateEdgeColour edge (x : xs) =
      let newEdge = if edge == x then setEdgeColour c x else x
       in newEdge : updateEdgeColour edge xs

digraphCycle :: (Eq a) => Digraph a -> Maybe (Path a)
digraphCycle Digraph {digraphEdges = edges} = digraphCycle' $ fmap (\e -> (edgeStart e, [e])) edges
  where
    -- We are building paths in reverse order
    pathEnd' p = edgeEnd $ head p
    digraphCycle' [] = Nothing
    digraphCycle' paths = case findCycleInPaths paths of
      Nothing -> digraphCycle' (updatePaths paths)
      Just c  -> Just $ reverse c
    findCycleInPaths [] = Nothing
    findCycleInPaths ((n, p) : ps) =
      if n == pathEnd' p
        then Just p
        else findCycleInPaths ps
    updatePaths ps = updatePaths' ps []
    updatePaths' [] acc = acc
    updatePaths' ((n, p) : ps) acc =
      let newEdges = filter ((pathEnd' p ==) . edgeStart) edges
       in updatePaths' ps (fmap (\e -> (n, e : p)) newEdges ++ acc)

digraphFromAlphabeticalOrder :: [String] -> Digraph Text
digraphFromAlphabeticalOrder ws = digraphFromAlphabeticalOrder' ws (Digraph [])
  where
    digraphFromAlphabeticalOrder' (x : y : xs) g = digraphFromAlphabeticalOrder' (y : xs) (updateDigraph x y g)
    digraphFromAlphabeticalOrder' _ g = g
    updateDigraph x y g =
      case compareStrings (splitToGraphemes $ pack x) (splitToGraphemes $ pack y) of
        Just (a, b) -> g {digraphEdges = mkEdge a b : digraphEdges g}
        Nothing     -> g
    splitToGraphemes s = breaks (breakCharacter Root) s
    compareStrings (a : as) (b : bs) =
      if a == b
        then compareStrings as bs
        else Just (brkBreak a, brkBreak b)
    compareStrings _ _ = Nothing
