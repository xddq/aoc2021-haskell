{-|
Example data:
    2199943210
    3987894921
    9856789892
    8767896789
    9899965678
        - Each number corresponds to the height of a particular location, where 9
is the highest and 0 is the lowest a location can be.
        - Find the low points (locations where any other adjacent (horizontal or
        vertical) point has a value that is lower than the current(EQ does not
count as lower).
        - In the above example, there are four low points
        - risk level of a low point is 1 plus its height
        - get the sum of risk levels of all low points.
-}
-- for using Map. src for notation:
-- https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html
import Data.Map (Map)
import qualified Data.Map as M

-- for splitOn
import Data.List.Split

{-|
Initial Plan:
    - Build an easy to use data structure. Read input and build a grid from it.
    - Use zipping to create the following lists:
    - Use all rows of that grid to create a list of tuples (Just left, point,
Just right).
    - Use all cols of that grid (with transpose grid) to create a list of
tuples(Just above, point, Just below).
    - Zip these to get a list of tuples with (Just left, Just above, point, Just
right, Just below)
    - fold the grid to get the result (remember adding +1 for each point)
-}
main = do
  input <- lines <$> readFile "inputDay9"
  -- NOTE(pierre): read does infer it's type by knowing ex1 takes list of Int
  print $ ex1 $ read input
  -- print $ ex2 input
  return ()

ex1 :: [Int] -> Int
ex1 row -> 1
