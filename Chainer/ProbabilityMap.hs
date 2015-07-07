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

module Chainer.ProbabilityMap where
import qualified Chainer.Probability as Probability
import qualified Chainer.HistogramMap as HistogramMap
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified System.Random as Random
import Data.List (foldl')

type ProbabilityMap a = Map.Map a (Probability.Probability a)

{-
 - Creates an empty ProbabilityMap
-}
emptyPM :: ProbabilityMap a
emptyPM = Map.empty :: Map.Map a (Probability.Probability a)

{-
 - Creates a ProbabilityMap from a list
-}
fromListPM :: Ord a => [a] -> ProbabilityMap a
fromListPM lst = fromHistogramMap (HistogramMap.fromListHM lst)

{-
 - Creates a ProbabilityMap from a HistogramMap
-}
fromHistogramMap :: Ord a => HistogramMap.HistogramMap a -> ProbabilityMap a
fromHistogramMap h = foldl' (\hAcc hKey -> (let histogram = HistogramMap.histogram hKey  h in 
                                              let probability = Probability.fromHistogram histogram in
                                                  adjustOrInsertPM (\p -> Probability.addP hKey histogram probability) hKey hAcc)) 
                                                  emptyPM (Map.keys h)

{-
 - Merges 2 ProbabilityMap's together
-}
mergePM :: Ord a => ProbabilityMap a -> ProbabilityMap a -> ProbabilityMap a
mergePM p1 p2 = Map.unionWith (\x1 x2 -> Probability.mergeP x1 x2) p1 p2
                             
{-
 - Inserts k into the ProbabilityMap if it doesn't exist, otherwise adjusts
 - the value of k by applying f to it.
-}
adjustOrInsertPM :: Ord k => (Probability.Probability k -> Probability.Probability k) -> k -> ProbabilityMap k -> ProbabilityMap k
adjustOrInsertPM f k p = case (Map.lookup k p) of
                           Just found -> Map.adjust f k p
                           Nothing -> Map.insert k (f Probability.emptyP) p

{-
 - Picks the successor of a based off of the ProbabilityMap. If
 - a is not found in the probability map then a random successor
 - will be chosen.
-}
pickPM :: (Ord a, Random.RandomGen g) => a -> ProbabilityMap a -> g -> (a, g)
pickPM a p  g = case (Map.lookup a p) of
                 Just x -> (Probability.pick x g)
                 -- Picks a random element from the probability map
                 Nothing -> (Probability.pick (Maybe.fromJust(Map.lookup (head (Map.keys p)) p)) g)
