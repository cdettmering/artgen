{-
This file is part of artgen.

Foobar is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Foobar is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

Copyright (c) 2015 Chad Dettmering

Authors:
    Chad Dettmering     chad.dettmering@gmail.com
-}

module Chainer.Probability where
import qualified Chainer.HistogramMap as HistogramMap
import qualified Chainer.Histogram as Histogram
import qualified Data.Map.Strict as Map
import System.Random as Random
import Data.List (foldl')


-- Maps an element to the probability of that element occurring next.
type Probability a = Map.Map a Float

{-
 - Creates an empty Probability
-}
emptyP :: Probability a
emptyP = Map.empty :: Map.Map a Float

{-
 - Creates a Probability from a Histogram
-}
fromHistogram :: Ord a => Histogram.Histogram a -> Probability a
fromHistogram histogram = foldl' (\acc element -> addP element histogram acc) emptyP (Map.keys histogram)

{-
 - Merges 2 Probability's together
-}
mergeP :: Ord a => Probability a -> Probability a -> Probability a
mergeP p1 p2 = Map.unionWith (+) p1 p2

{-
 - Adds value to the Probability, or adjust the value
 - if it already exists in the Probability
-}
addP :: Ord a => a -> Histogram.Histogram a -> Probability a -> Probability a
addP value histogram probability = adjustOrInsertP (\element -> calculate value histogram) value probability

{-
 - Inserts value into the Probability if it doesn't exist, otherwise adjusts
 - the value of value by applying f to it. Automatically drops 0 probabilities.
-}
adjustOrInsertP :: Ord k => (Float -> Float) -> k -> Probability k -> Probability k
adjustOrInsertP f value probability = case (Map.lookup value probability) of
                           Just found -> Map.adjust f value probability
                           -- Don't include 0 probability
                           Nothing -> let prob = (f 0) in if prob /= 0 then Map.insert value (f 0) probability else probability

{-
 - Calculates the probability of element occurring based off of the number of occurrences
 - of element in the Histogram.
-}
calculate :: Ord a => a -> Histogram.Histogram a -> Float
calculate element histogram = case (Histogram.sumOccurrences histogram) of
                                  0 -> 0
                                  x -> (fromInteger (Histogram.occurrences element histogram)) / (fromInteger x)

{-
 - Gets the probability of element occurring from the Probability
-}
probability :: Ord a => a -> Probability a -> Float
probability element probability = case (Map.lookup element probability) of
                                      Just prob -> prob
                                      Nothing -> 0

{-
 - Pads the list out to the length given using the last element in the
 - list
-}
padList :: Int -> [a] -> [a]
padList size lst = lst ++ (take (size - (length lst)) (cycle [last lst]))

{-
 - Generates a "pick list" from the Probability. See
 - pick for a more detailed explanation.
-}
pickList :: Ord a => Probability a -> [a]
pickList p = padList 500 (foldl' (\acc x -> (pickSubList x (probability x p)) ++ acc) [] (Map.keys p))

{-
 - Generates a pick sub list for a specific element. These sub lists are then
 - concat'd together to create the final list.
-}
pickSubList :: a -> Float -> [a]
pickSubList element probability = let numElements = (floor (probability * 500)) in take numElements (cycle [element])

{-
 - Picks a random element from the list
-}
pickFromList :: Random.RandomGen g => [a] -> g -> (a, g)
pickFromList lst gen  = let (a, g) = Random.randomR (0, 499) gen in (lst !! a, g)

{-
 - Picks a random value from the Probability based off of each values
 - probability to be picked. This is done through a "pick list". The list
 - consists of 500 elements, with each value in the Probability taking up
 - the probability equivalent number of elements in the list. Then a random
 - element from the list is chosen.
 -
 - For example: If the probability contain the values: 1 -> 10%, 2 -> 15%, 3 -> 75%
 - then the pick list will contain 50 1's, 75 2's and 375 3's. This support a
 - probability resolution down to .03%
-}
pick :: (Ord a, Random.RandomGen g) => Probability a -> g -> (a, g)
pick p g = pickFromList (pickList p) g
