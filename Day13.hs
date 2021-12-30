{-|
Example data:
-}
import Data.List (foldl')
import Data.List.Split (splitOn)

import Data.Map (Map)
import qualified Data.Map as M

main
  -- input <- lines <$> readFile "inputDay11"
 = do
  input <- lines <$> readFile "inputDay13"
  print $ ex1 input
  print $ ex2 input
  return ()

type Pos = Int

data FoldMode
  = Vertical
  | Horizontal
  deriving (Show, Read, Eq)

type Dots = Map Int (Map Int Int)

ex1 :: [[Char]] -> Int
ex1 input =
  let (dots, _:instructionsString) = break (== "") input
      dotMap
      -- TODO(pierre): Why do I need to wrap mkDot $ splitOn ... into []?
      -- Probably desugar list expression later to understand it.
       = mkDotMap $ [(x, y) | dot <- dots, (x, y) <- [mkDot $ splitOn "," dot]]
      instructions = map parseInstruction instructionsString
   in mapOfMapsCount $ foldPaper dotMap $ head instructions

-- ex2 :: [[Char]] -> Int
ex2 input =
  let (dots, _:instructionsString) = break (== "") input
      dotMap =
        mkDotMap $ [(x, y) | dot <- dots, (x, y) <- [mkDot $ splitOn "," dot]]
      instructions = map parseInstruction instructionsString
   in foldl' foldPaper dotMap instructions

-- TODO(pierre): print the grid to see letters?
--
mapOfMapsCount :: Map Int (Map Int Int) -> Int
mapOfMapsCount = M.foldl (\acc innerMap -> acc + M.size innerMap) 0

parseInstruction :: String -> (FoldMode, Pos)
parseInstruction instruction =
  let axis:_:amount = last $ words instruction
   in (parseMode axis, read amount)

parseMode :: Char -> FoldMode
parseMode input =
  case input of
    'x' -> Horizontal
    'y' -> Vertical
    otherwise ->
      error $
      "This case should not happen. parseMode function got input: " ++ [input]

-- folding from right to left(x-wise) and from bottom to top (y-wise)
foldPaper :: Dots -> (FoldMode, Pos) -> Dots
foldPaper dots (mode, foldPos)
  | mode == Vertical
    -- MAYBE(pierre): have to map over all values multiple times. later find way
    -- to avoid.
   =
    let upperAndLowerMaps = M.map (M.split foldPos) dots
        -- get upper maps (maps/dots above the cut) and lower maps (maps/dots below
        -- the cut)
        -- get upper maps by just taking the upper ones.
        upperMaps =
          M.filter (not . null) $ M.map (\(upper, _) -> upper) upperAndLowerMaps
        -- get new upper maps by moving the lower keys/dots according to their
        -- position and the folding position.
        newUpperMaps =
          M.filter (not . null) $
          M.map (M.filterWithKey (\k _ -> k /= -10)) $
          M.map (M.mapKeys (adaptPos foldPos)) dots
        -- make union of both maps/dots to mimic overlapping of dots.
        resultDots = M.unionWith (\x y -> M.union x y) newUpperMaps upperMaps
     in resultDots
  | mode == Horizontal =
    let (leftMap, rightMap) = M.split foldPos dots
        newLeftMap
          -- MAYBE: lookup filter for Map if this makes troubles.
         -- get new left maps by moving the keys according to their position and
         -- the folding position.
         = M.mapKeys (adaptPos foldPos) rightMap
        -- make union of both maps/dots to mimic overlapping of dots.
        resultDots = M.unionWith (\x y -> M.union x y) newLeftMap leftMap
     in resultDots

adaptPos :: Int -> Int -> Int
adaptPos cut pos =
  let diff = pos - cut
      newPos = cut - diff
   in if cut > pos || newPos < 0
        then -10
        else newPos

-- expects given list of strings to be something like ["1","12"] and transforms
-- it into (1,12)
mkDot :: [String] -> (Int, Int)
mkDot (x:y:_) = (read x, read y)

mkDotMap :: [(Int, Int)] -> Map Int (Map Int Int)
mkDotMap = foldl' addEntry M.empty

addEntry :: Map Int (Map Int Int) -> (Int, Int) -> Map Int (Map Int Int)
addEntry input (x, y) =
  case M.lookup x input of
    Nothing -> M.insert x (M.insert y 1 M.empty) input
    Just yMap -> M.insert x (M.insert y 1 yMap) input
