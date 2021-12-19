{-|
Example data:
-}
import Data.List (foldl', intersect)
import Data.List.Split

import Data.Map (Map)
import qualified Data.Map as M

import Data.Char

main = do
  input <- lines <$> readFile "inputDay12"
  print $ ex1 input
  print $ ex2 input
  return ()

type Node = [Char]

type Path = [Node]

type Count = Int

type EdgeMap = Map Node [Node]

ex1 :: [[Char]] -> Count
ex1 input =
  length $
  -- map (tail . reverse) $
  filter (not . null) $ walk "start" [[]] useNode $ mkEdgeMap input

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

endNode = "end"

startNode = "start"

emptyPath :: Path
emptyPath = []

walk :: Node -> Path -> (Node -> Path -> Bool) -> Map Node [Node] -> [Path]
walk node visited pred edgeMap =
  if node == endNode
    then [node : visited]
    else case pred node visited of
           True ->
             case M.lookup node edgeMap of
               Just stepableNodes ->
                 concatMap
                   (\nextNode -> walk nextNode (node : visited) pred edgeMap)
                   stepableNodes
               Nothing -> [emptyPath]
           otherwise -> [emptyPath]

-- Determines whether we can use this node for walking towards the goal.
useNode :: Node -> Path -> Bool
useNode node path
  | all isLower node && node `elem` path = False
  | otherwise = True

ex2 :: [[Char]] -> Count
ex2 input =
  length $
  -- map (tail . reverse) $
  filter (not . null) $ walk startNode [[]] useNode2 $ mkEdgeMap input

-- Determines whether we can use this node for walking towards the goal.
useNode2 :: Node -> Path -> Bool
useNode2 node path
  -- not in path yet
  | (not $ node `elem` path) = True
  -- once in path already
  | otherwise =
    if node `elem` [startNode, endNode]
      then False
      -- once in path and not start or endnode
      else if all isLower node
             -- we have a lowercase letter twice in path already
             then not $ twiceInPath $ filter (all isLower) path
             else True

twiceInPath path = existsTwice path

existsTwice [] = False
existsTwice (x:xs)
  | x `elem` xs = True
  | otherwise = existsTwice xs
