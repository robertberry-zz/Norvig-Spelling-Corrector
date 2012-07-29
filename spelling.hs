
import System.Environment

import qualified Data.Map as Map
import qualified Data.Set as Set
import List (sort, group, maximumBy)
import Data.Ord (comparing)
import Data.Maybe

-- Given a sequence, returns a list of tuples of distinct items and the number
-- of times each distinct item appears in sequence
  
occurrences :: Ord x => [x] -> [(x, Int)]
occurrences = map (\(elems) -> (head elems, length elems))  . group . sort

-- Given a set of features, returns a dictionary ...

type CountMap x = Map.Map x Int
type TrainingSet = CountMap [Char]

train ::[[Char]] -> TrainingSet
train = Map.fromList . occurrences

-- The alphabet!
alphabet = ['a'..'z']

type WordSet = Set.Set [Char]

-- Returns a set of edits for the given word, composed of either a single
-- deletion, transposition, replacement or insertion
edits1 :: [Char] -> WordSet
edits1 word = Set.fromList (deletes ++ transposes ++ replaces ++ inserts)
  where splits = map (`splitAt` word) [0..(length word)]
        deletes = [a ++ (tail b) | (a, b) <- splits, not (null b)]
        transposes = [a ++ [b !! 1] ++ [b !! 0] ++ (drop 2 b) |
                      (a, b) <- splits, length b > 1]
        replaces = [a ++ [c] ++ (tail b) | (a, b) <- splits, c <- alphabet,
                    not (null b)]
        inserts = [a ++ [c] ++ b | (a, b) <- splits, c <- alphabet]

-- Returns a set of edits for the given word, composed of 2 edits (see edits1).
edits2 :: [Char] -> WordSet
edits2 word = Set.unions [edits1 edit | edit <- Set.toList (edits1 word)]

-- Given a training set and a list of words, returns a list of those words
-- that were found in the training set
known :: TrainingSet -> [[Char]] -> [[Char]]
known set = filter (`Map.member` set)

some :: (a -> Bool) -> [a] -> Maybe a
some _ [] = Nothing
some pred (x:xs) = if pred x then Just x else some pred xs

correct :: TrainingSet -> [Char] -> [Char]
correct set word = maximumBy (comparing (\k -> Map.findWithDefault 0 k set))
                   candidates
                   where (Just candidates) = some (\cs -> (length cs) > 0)
                                             (map (known set) [[word],
                                                               Set.toList (edits1 word),
                                                               Set.toList (edits2 word)])

main = do
  (trainingFile:(word:_)) <- getArgs
  trainingText <- readFile trainingFile
  let features = words trainingText
  putStr $ correct (train features) word
  putStr "\n"
