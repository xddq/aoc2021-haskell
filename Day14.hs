{-|
- Example data:
    Template:     NNCB
    After step 1: NCNBCHB
    After step 2: NBCCNBBBCBHCB
    After step 3: NBBBCNCCNBBNBNBBCHBHHBCHB
    After step 4: NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB
- every 2 letters count. Example: if we have 3 letters(NCB) that would be 2 base
pairs(NC;CB) which could result in a new base.
-}
import Data.Map (Map)
import qualified Data.Map as M

import Data.List (foldl')
import Data.List.Split (splitOn)

type Sequence = [Element]

type Element = Char

type PolymerMap = Map Sequence Element

type StepCount = Int

main
  -- input <- lines <$> readFile "inputDay11"
 = do
  input <- lines <$> readFile "testInput"
  print $ ex1 input
  -- print $ ex2 input
  return ()

-- ex1 :: [[Char]] -> Int
ex1 input =
  let ([start], _:sequences) = break (== "") input
  --               -- try to solve with uncurry?
      polymerMap =
        foldl'
          (\acc x ->
             let (input, output) = mkRule x
              in M.insert input output acc)
          M.empty
          sequences
   in polymerInsertion start polymerMap

-- runs one full step of polymerInsertion
polymerInsertion :: Sequence -> PolymerMap -> Sequence
polymerInsertion (x:[]) _ = [x]
polymerInsertion (x:y:rest) zs =
  case M.lookup [x, y] zs of
    Just elem -> x : elem : polymerInsertion (y : rest) zs
    Nothing -> x : polymerInsertion (y : rest) zs

-- expects input to be in form of "AB -> C" to create ("AB",'C')
mkRule :: [Char] -> (Sequence, Element)
mkRule input =
  let ((e1:e2):_:e3) = words input
   in (e1 : e2, head $ last e3)
