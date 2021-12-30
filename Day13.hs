{-|
Example data:
-}
import Data.List (foldl', transpose)
import Data.List.Split (splitOn)

import Data.Map (Map)
import qualified Data.Map as M

main = do
  input <- lines <$> readFile "inputDay13"
  print $ ex1 input
  printMapOfMaps $ ex2 input
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
   in fillZeros $ foldl' foldPaper dotMap instructions

printMapOfMaps :: Map Int (Map Int Int) -> IO ()
printMapOfMaps dots =
  mapM_ print $
  transpose $
  map snd $ M.toList $ M.map (\innerMap -> map snd $ M.toList innerMap) dots

-- adds zeros to a given map of maps. Prepares data for pretty printing to get
-- the code (our solution).
fillZeros :: Map Int (Map Int Int) -> Map Int (Map Int Int)
fillZeros mapOfMaps
  -- Fills all given inner maps with zeros if the value for the given index did
  -- not exist.
 =
  M.map
    (\innerMap ->
       foldl'
         (\acc key ->
            M.insertWith
              (\givenInput currentVal ->
                 if currentVal == 1
                   then currentVal
                   else givenInput)
              key
              0
              acc)
         innerMap
         [0 .. 5]) $
  -- Adds an empty map to each key that does not contain a map already.
  foldl'
    (\dots key ->
       M.insertWith
         (\givenInput currentVal ->
            if Prelude.null currentVal
              then givenInput
              else currentVal)
         key
         (M.fromList [])
         dots)
    mapOfMaps
    [0 .. 40]

-- Gets the counts of all elements of a given map of maps.
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

-- folding from right to left(for x-axis) and from bottom to top (for y-axis)
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
