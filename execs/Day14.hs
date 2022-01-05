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
module Main
  ( main
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.List (foldl', sortBy)
import Data.List.Split (splitOn)

type Sequence = [Element]

type Element = Char

type Count = Int

data Part
  = PartOne
  | PartTwo

main :: IO ()
main = do
  rawInput <- lines <$> readFile "./inputs/testInput"
  let input = (parse rawInput)
  print $ solve input 10
  print $ solve input 40
  return ()

-- Returns the starting polymer and the rules for polymerization
parse :: [[Char]] -> (Sequence, Map Sequence Char)
parse input = (inputPolymer, rules)
  where
    ([inputPolymer], _:sequences) = break (== "") input
    rules =
      foldl'
        (\acc seq ->
           (\rule@(sequence, element) -> M.insert sequence element acc) $
           mkRule seq)
        M.empty
        sequences

-- Uses map with keys being sequences (e.g. "NN" or "NB" ...) and values the
-- amount/count of them.
solve :: (Sequence, Map Sequence Char) -> Count -> Int
solve (inputPolymer, rules) steps =
  let polymerCounts
      -- Parses polymer (e.g. "NNCB") into [("NN",1),("NC",1),...]
       =
        M.fromListWith (+) $
        zipWith (\x y -> ([x, y], 1)) inputPolymer $ tail inputPolymer
      -- Remembers first elem (which would be lost). See transformSequence.
      firstElem = head inputPolymer
   in (\counts -> abs $ snd (head counts) - snd (last counts)) $
      sortBy (\(_, count1) (_, count2) -> compare count1 count2) $
      M.toList $
      M.insertWith (+) firstElem 1 $
      countElements $ runXtimes (polymerize rules) polymerCounts steps

-- Expects input to be in form of "AB -> C" to create ("AB",'C')
mkRule :: [Char] -> (Sequence, Element)
mkRule input =
  let ((e1:e2):_:e3) = words input
   in (e1 : e2, head $ last e3)

-- Runs one step of polymerization
polymerize :: Map Sequence Element -> Map Sequence Count -> Map Sequence Count
polymerize mappings counts =
  M.fromListWith (+) $ concatMap (transformSequence mappings) $ M.toList counts

-- Transfroms given sequence (e.g. ("NN",100)) according to given mappings. If
-- we had the mapping ("NN",'B') the result would be [("NB",100),("BN",100)].
-- This will end up doubling our elements. In this example we would have two
-- times "B" but we only have one. That's why we count only the second elem in
-- countElements later.
transformSequence ::
     Map Sequence Element -> (Sequence, Count) -> [(Sequence, Count)]
transformSequence mappings (seq, count) =
  case M.lookup seq mappings of
    Just val -> [(head seq : [val], count), (val : tail seq, count)]
    Nothing -> [(seq, count)]

-- Counts every second element of our sequence (e.g. ("NN",100) will be
-- ('N',100). Did this because otherwise we would count elements twice.
countElements :: Map Sequence Count -> Map Element Count
countElements =
  M.foldlWithKey
    (\elemCounts (e1:e2:_) count -> M.insertWith (+) e2 count elemCounts)
    M.empty

-- counts number of occurences for each element in a given sequence.
countOccurences :: Map Element Count -> Sequence -> Map Element Count
countOccurences = foldl (\countMap x -> M.insertWith (+) x 1 countMap)

-- src: https://stackoverflow.com/questions/7423123/how-to-call-the-same-function-n-times/7423199
runXtimes :: (a -> a) -> a -> Int -> a
runXtimes f x times = iterate f x !! times
