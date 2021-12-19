{-|
Example data:
-}
import Data.List (foldl')
import Data.List.Split

import Data.Map (Map)
import qualified Data.Map as M

import Data.Char

main
  -- input <- lines <$> readFile "inputDay11"
 = do
  input <- lines <$> readFile "testInput"
  -- print $ ex1 input
  print $ ex2 input
  return ()

type Node = [Char]

type Path = [Node]

type Count = Int

type EdgeMap = Map Node [Node]

ex1 :: [[Char]] -> Count
ex1 input =
  length $
  map (tail . reverse) $
  filter (not . null) $ walk "start" [[]] $ mkEdgeMap input

mkEdgeMap :: [[Char]] -> EdgeMap
mkEdgeMap input
      -- parses input which looks like ["x-y",..] to "["x","y"],..] where x and
      -- y will be an edge with x being the start and y the end node.
 =
  let edges = map (splitOn "-") input
        -- Creates bidirectional Map of edges consisting of the individual
        -- nodes as keys and all the nodes they are connected with as
        -- values.
   in M.fromListWith (++) $
      concat $
      [ [(head edge, tail edge), (head $ tail edge, [head edge])]
      | edge <- edges
      ]

-- ex1 input = mkEdgeMap input
endNode = "end"

startNode = "start"

emptyPath :: Path
emptyPath = []

walk :: Node -> Path -> Map Node [Node] -> [Path]
walk node visited edgeMap =
  if node == endNode
    then [node : visited]
    else case useNode node visited of
           True ->
             case M.lookup node edgeMap of
               Just stepableNodes ->
                 concatMap
                   (\nextNode -> walk nextNode (node : visited) edgeMap)
                   stepableNodes
               Nothing -> [emptyPath]
           otherwise -> [emptyPath]

-- Determines whether we can use this node for walking towards the goal.
useNode :: Node -> Path -> Bool
useNode node path
  | all isLower node && node `elem` path = False
  | otherwise = True

-- ex2 :: [[Char]] -> Int
ex2 input
  -- length $
  -- nub $
 =
  length $
  map (tail . reverse) $
  filter (not . null) $ walk2 startNode [[]] $ mkEdgeMap input

walk2 :: Node -> Path -> Map Node [Node] -> [Path]
walk2 node visited edgeMap =
  if node == endNode
    then [node : visited]
    else case useNode2 node visited of
           True ->
             case M.lookup node edgeMap of
               Just stepableNodes ->
                 concatMap
                   (\nextNode -> walk2 nextNode (node : visited) edgeMap)
                   stepableNodes
               Nothing -> [emptyPath]
           otherwise -> [emptyPath]

-- Determines whether we can use this node for walking towards the goal.
useNode2 :: Node -> Path -> Bool
useNode2 node path
  | node `elem` [startNode, endNode] && (not $ node `elem` path) = True
  | and [all isLower node, visitedTwice path] = False
  | otherwise = True

canBeVisitedTwice :: Node -> Bool
canBeVisitedTwice node
  | node `elem` [startNode, endNode] = False
  | otherwise = True

-- MAYBE(pierre): this has bad runtime. Could adapt arguments of useNode2 and
-- walk2 and pass a flag to fix this after solution is done.
visitedTwice :: Path -> Bool
visitedTwice [] = False
visitedTwice (x:xs)
  | all isLower x && x `elem` xs = True
  | otherwise = visitedTwice xs
