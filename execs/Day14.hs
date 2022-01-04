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

import Control.Applicative (liftA2)

type Sequence = [Element]

type Element = Char

type Count = Int

data Part
  = PartOne
  | PartTwo

main :: IO ()
main = do
  input <- lines <$> readFile "./inputs/inputDay14"
  print $ ex2 input PartOne
  print $ ex2 input PartTwo
  return ()

-- for ex2 we need better management of our data structure. use map with keys
-- being all base pairs we have and the values of the keys the amount of these
-- base pairs we currently have.
ex2 :: [[Char]] -> Part -> Int
ex2 input part =
  let ([start], _:sequences) = break (== "") input
      polymerMappings =
        foldl'
          (\acc x ->
             let (input, output) = mkRule x
              in M.insert input output acc)
          M.empty
          sequences
      -- Parses starting sequence (e.g. "NNCB") into a map with each unique
      -- sequence as key and their count as value.
      polymerCounts =
        M.fromListWith (+) $ zipWith (\x y -> ([x, y], 1)) start $ tail start
      -- Remember first elem since we only count the second elems of our base
      -- pairs.
      firstElem = head start
      steps =
        case part of
          PartOne -> 10
          PartTwo -> 40
   in (\counts -> abs $ (snd $ head counts) - (snd $ last counts)) $
      sortBy (\(_, count1) (_, count2) -> compare count1 count2) $
      M.toList $
      M.insertWith (+) firstElem 1 $
      countElements $
      foldl'
        (\counts _ -> polymerize polymerMappings counts)
        polymerCounts
        [1 .. steps]

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
countElements seqCounts =
  M.foldlWithKey
    (\elemCounts (e1:e2:_) count -> M.insertWith (+) e2 count elemCounts)
    M.empty
    seqCounts

-- NOTE: now unused stuff I did for ex1. Just keep it for reference.
type PolymerMap = Map Sequence Element

ex1 :: [[Char]] -> Part -> Int
ex1 input part =
  let ([start], _:sequences) = break (== "") input
      polymerMap =
        foldl'
          (\acc x ->
             let (input, output) = mkRule x
              in M.insert input output acc)
          M.empty
          sequences
      -- runs polymerisation, counts occurences of each elements and sorts them
      -- (lowest first)
      elementCounts =
        sortBy (\(_, count1) (_, count2) -> compare count1 count2) $
        M.toList $
        (flip countOccurences) M.empty $
        runXtimes ((flip polymerInsertion) polymerMap) start (getStepCount part)
   in getDiff $ elementCounts
  where
    getStepCount part =
      case part of
        PartOne -> 10
        -- partTwo was not going to terminate with this data structure :D
        PartTwo -> 40

-- counts number of occurences for each element in a given sequence.
countOccurences :: Sequence -> Map Element Count -> Map Element Count
countOccurences [] countMap = countMap
countOccurences (x:rest) countMap =
  countOccurences rest $ M.insertWith (+) x 1 countMap

-- src for this:
-- https://stackoverflow.com/questions/7423123/how-to-call-the-same-function-n-times/7423199
runXtimes :: (a -> a) -> a -> Int -> a
runXtimes f x times = iterate f x !! times

-- runs one full step of polymerInsertion
polymerInsertion :: Sequence -> PolymerMap -> Sequence
polymerInsertion (x:[]) _ = [x]
polymerInsertion (x:y:rest) zs =
  case M.lookup [x, y] zs of
    Just elem -> x : elem : polymerInsertion (y : rest) zs
    Nothing -> x : polymerInsertion (y : rest) zs

-- NOTE(pierre): keep this stuff around here so I can check back if I run into
-- this again.
-- We have to define how we want Int as a Monoid ourselves (Post said since
-- mappend does not have a 'clear' default implementation. src:
-- https://stackoverflow.com/questions/29499119/why-int-does-not-implement-monoid/29499203
-- NOTE(pierre): need to have mappend in semigroup as definition was moved.
-- src: https://coderedirect.com/questions/392285/a-basic-monoid-definition-gives-no-instance-for-semigroup-mymonoid-arising-fr
-- TODO(pierre): why is this decided for some typeclasses and left open for
-- others?)
instance Monoid Int where
  mempty = 0

-- mappend is synonym of <>, but <> is required to be defined in semigroup.
instance Semigroup Int where
  x <> y = x

instance Monoid Char where
  mempty = '_'

instance Semigroup Char where
  x <> y = x

test1 :: (Int, Int) -> (Int, Int) -> (Int, Int)
test1 x y = (+) <$> x <*> y

test2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
test2 x y = liftA2 (+) x y

-- gets difference of the count for the first(smalles) and last(biggest) given
-- element.
getDiff :: [(Element, Count)] -> Int
getDiff xs = abs $ snd $ subtract <$> head xs <*> last xs
