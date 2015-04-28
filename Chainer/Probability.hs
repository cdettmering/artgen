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
import qualified Chainer.HistogramMap as HM
import qualified Chainer.Histogram as H
import qualified Data.Map as M
import System.Random as R
import System.IO.Unsafe as X

-- Maps an element to the probability of that element occurring next.
type Probability a = M.Map a Float

-- Lower and upper probability bounds
data Bounded a = Bounded {ord :: a, lower :: Float, upper :: Float}

{-
 - Creates an empty Probability
-}
empty :: Probability a
empty = M.empty :: M.Map a Float

{-
 - Creates a Probability from a Histogram
-}
fromHistogram :: Ord a => H.Histogram a -> Probability a
fromHistogram h = foldr (\x acc -> add x h acc) empty (M.keys h)

{-
 - Merges 2 Probability's together
-}
merge :: Ord a => Probability a -> Probability a -> Probability a
merge p1 p2 = M.unionWith (+) p1 p2

{-
 - Adds a value to the Probability, or adjust the value
 - if it already exists in the Probability
-}
add :: Ord a => a -> H.Histogram a -> Probability a -> Probability a
add a h p = adjustOrInsert (\x -> calculate a h) a p

{-
 - Inserts k into the Probability if it doesn't exist, otherwise adjusts
 - the value of k by applying f to it. Automatically drops 0 probabilities.
-}
adjustOrInsert :: Ord k => (Float -> Float) -> k -> Probability k -> Probability k
adjustOrInsert f k p = case (M.lookup k p) of
                           Just found -> M.adjust f k p
                           -- Don't include 0 probability
                           Nothing -> let probability = (f 0) in if probability /= 0 then M.insert k (f 0) p else p

{-
 - Calculates the probability of a occurring based off of the number of occurrences
 - of a in the Histogram.
-}
calculate :: Ord a => a -> H.Histogram a -> Float
calculate a h = case (H.sumOccurrences h) of
                    0 -> 0
                    x -> (fromInteger (H.occurrences a h)) / (fromInteger x)

{-
 - Gets the probability of a occurring from the Probability
-}
probability :: Ord a => a -> Probability a -> Float
probability a p = case (M.lookup a p) of
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
pickList p = padList 500 (foldr (\x acc -> (pickSubList x (probability x p)) ++ acc) [] (M.keys p))

{-
 - Generates a pick sub list for a specific element. These sub lists are then
 - concat'd together to create the final list.
-}
pickSubList :: a -> Float -> [a]
pickSubList a p = let numElements = (floor (p * 500)) in take numElements (cycle [a])

{-
 - Picks a random element from the list
-}
pickFromList :: R.RandomGen g => [a] -> g -> (a, g)
pickFromList lst gen  = let (a, g) = R.randomR (0, 499) gen in (lst !! a, g)

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
pick :: (Ord a, R.RandomGen g) => Probability a -> g -> (a, g)
pick p g = pickFromList (pickList p) g
